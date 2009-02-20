(* Dan Blanchard
   Segmentation Framework *)

open Pcre
open Printf
open ExtList
open ExtString
open ExtArray

let printUtteranceDelimiter = ref false
let displayLineNumbers = ref false
let featureFile = ref ""
let badScore =  ref 0.0
let initialNgramCount = ref 0.0000001
let wordDelimiter = ref " "
let utteranceDelimiter = ref "$"
let corpus = ref ""
(* let sentenceList = ref [] *)
let lexiconOut = ref ""
let phonemeCountsOut = ref ""
let lexicon = Hashtbl.create 10000

let sixOverPiSquared = 6.0 /. (3.1415926536 ** 2.0)
let removeSpacesPattern = regexp "((\\s)|(\\.))+"
let windowSize = ref 1
let jointProb = ref false
let smooth = ref false
let tokenPhonotactics = ref false
let totalWords = ref 0

(* Framework components: search algorithm, evidence/cues, method for combining evidence, method for updating stats, corpus processor *)

module type CUE =
sig
	(* Returns negative log probability that proposed word is a word.
	 * Takes function that determines how to combine scores in the case of sub-scores as argument. *)
	val eval_word : string -> (float -> float -> float) -> float
	
	(* Takes a word and updates any evidence the cue keeps track of based on another occurrence of this word. *)
	val update_evidence : string -> unit
end

module FamiliarWordCue : CUE = 
struct
	let totalWords = ref 0
	
	(* Returns negative log of frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		let wordCountFloat = float (Hashtbl.find lexicon word) in
		let wordTypesFloat = float (Hashtbl.length lexicon) in
		let totalWordsFloat = float !totalWords in
		-.(log (wordCountFloat /. (totalWordsFloat +. wordTypesFloat)))
		
	let update_evidence (newWord:string) = 
		if Hashtbl.mem lexicon newWord then
			Hashtbl.replace lexicon newWord ((Hashtbl.find lexicon newWord) + 1)
		else
			Hashtbl.add lexicon newWord 1;;
			
	
end

module PhonemeNgramCue : CUE = struct
	let ngramCountsArray = Array.init (!windowSize) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))))
	let totalNgramsArray = Array.init (!windowSize) (fun a -> 0.0)
	let typesWithCountArray = Array.init 3 (fun a -> 0)
	let ngramList = List.init !windowSize (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	
	(* Calculates D_n for Modified Kneser-Ney Smoothing*)
	let rec discount n wordTypesWithCountArray = 
		if (n = 0) then
			wordTypesWithCountArray.(0) / (wordTypesWithCountArray.(0) + (2 * wordTypesWithCountArray.(1)))
		else if (n < 3) then
			n - ((n + 1) * (discount 0 wordTypesWithCountArray) * (wordTypesWithCountArray.(n) / (wordTypesWithCountArray.(n - 1))))
		else
			3 - (4 * (discount 0 wordTypesWithCountArray) * (wordTypesWithCountArray.(4) / (wordTypesWithCountArray.(3))));;


	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram_conditional ngram n wordNgramCountsArray wordTotalNgramsArray =
		let prefix = String.sub ngram 0 n in
		if (n = 0) then
			(Hashtbl.find wordNgramCountsArray.(n) ngram) /. wordTotalNgramsArray.(n)
		else
			begin
				if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) /. (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) /. (Hashtbl.find ngramCountsArray.(n - 1) prefix)
						else
							!badScore
					end
				else 
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							(Hashtbl.find ngramCountsArray.(n) ngram) /. (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							(Hashtbl.find ngramCountsArray.(n) ngram) /. (Hashtbl.find ngramCountsArray.(n - 1) prefix)
						else
							!badScore
					end								
			end;;

	let prob_ngram_kneser_ney ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray = 
		let prefix = String.sub ngram 0 n in
		let d = discount n in
		if (n = 0) then
			(Hashtbl.find wordNgramCountsArray.(n) ngram) /. wordTotalNgramsArray.(n)
		else
			begin
				if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) /. (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) /. (Hashtbl.find ngramCountsArray.(n - 1) prefix)
						else
							!badScore
					end
				else 
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							(Hashtbl.find ngramCountsArray.(n) ngram) /. (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							(Hashtbl.find ngramCountsArray.(n) ngram) /. (Hashtbl.find ngramCountsArray.(n - 1) prefix)
						else
							!badScore
					end								
			end;;

	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram_joint ngram n wordNgramCountsArray wordTotalNgramsArray =
		(Hashtbl.find wordNgramCountsArray.(n) ngram) /. wordTotalNgramsArray.(n);;

	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray = 
		match (!jointProb, !smooth) with 
			(false, true)  -> prob_ngram_kneser_ney ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray
		|	(false, false) -> prob_ngram_conditional ngram n wordNgramCountsArray wordTotalNgramsArray
		| 	(_,_) -> prob_ngram_joint ngram n wordNgramCountsArray wordTotalNgramsArray;;
	
	(* Gets the substrings necessary for string extension learning *)
	let rec get_substrings windowVector word =
		match windowVector with
		| currentWindow::windowTail -> 
			let windowSum = (List.fold_left (+) 0 windowVector) in
			if ((String.length word) >= windowSum) then
				let firstCharList = List.init ((String.length word) - (windowSum - 1)) (fun a -> a) in
				List.flatten
					(List.map
						(fun firstChar -> 
							let prefix = String.sub word firstChar currentWindow in
							List.map
								(fun suffix ->
									prefix ^ suffix
								)
								(get_substrings windowTail (String.slice ~first:(firstChar + currentWindow) word))

						)
						firstCharList)
			else
				[""]
		| [] -> [""];;

	
	(* Returns negative log of frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		let wordNgramCountsArray = Array.init (!windowSize) (fun a -> Hashtbl.create 100) in
		let wordTotalNgramsArray = Array.init (!windowSize) (fun a -> 0.0) in
		let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in		
		let wordWithBoundary = (if !windowSize > 1 then 
									!wordDelimiter ^ word ^ !wordDelimiter 
								else 
									word ^ !wordDelimiter) in							
		if (String.length word) < !windowSize then
			-. (log !badScore)
		else	
			begin
				List.iter (* Get n-gram counts of all size *)
					(fun currentWindowSizeMinusOne ->
						let ngramFirstCharListLength = (String.length wordWithBoundary) - currentWindowSizeMinusOne in 
						let ngramFirstCharList = List.init ngramFirstCharListLength (fun a -> a) in
						List.iter (* Loop through all n-grams of current size *)
							(fun firstChar ->
								let ngram = String.sub wordWithBoundary firstChar (currentWindowSizeMinusOne + 1) in
								if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
									Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram (Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram)
								else
									Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram 0.0;
							)
							ngramFirstCharList;
						Array.set wordTotalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. (float ngramFirstCharListLength))
					)
					ngramList;
				let phonemeScore = ref (if !windowSize > 1 then 
											-.(log 1.0)
										else
											-.(log (1.0 /. (1.0 -. ((Hashtbl.find wordNgramCountsArray.(0) !wordDelimiter) /. wordTotalNgramsArray.(0)))))) in
				List.iter (* Get ngram scores *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar !windowSize in
						phonemeScore := (combine !phonemeScore (-. (log (prob_ngram ngram (!windowSize - 1) wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray))))
					)
					(List.init ((String.length wordWithBoundary) - (!windowSize - 1)) (fun a -> a));
				!phonemeScore
			end
		
	let update_evidence (newWord:string) = 
		let wordWithBoundary = (if !windowSize > 1 then 
									!wordDelimiter ^ newWord ^ !wordDelimiter 
								else 
									newWord ^ !wordDelimiter) in
		let wordWindow = (if (String.length wordWithBoundary) < !windowSize then
								String.length wordWithBoundary
						else
							!windowSize) in
		let wordNgramList = List.init wordWindow (fun a -> a) in
		List.iter (* Get n-gram counts of all size *)
			(fun currentWindowSizeMinusOne ->
				let ngramFirstCharListLength = (String.length wordWithBoundary) - currentWindowSizeMinusOne in 
				let ngramFirstCharList = List.init ngramFirstCharListLength (fun a -> a) in
				List.iter (* Loop through all n-grams of current size *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar (currentWindowSizeMinusOne + 1) in
						if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
							Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) +. 1.0)
						else
							Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram 1.0;
					)
					ngramFirstCharList;
				Array.set totalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. (float ngramFirstCharListLength))
			)
			wordNgramList

end



(*****BEGIN MAIN PROGRAM******)

(* Process command-line arguments *)
let process_anon_args corpusFile = corpus := corpusFile
let arg_spec_list =["--wordDelimiter", Arg.Set_string wordDelimiter, " Word delimiter";
					"-wd", Arg.Set_string wordDelimiter, " Short for --wordDelimiter";
					"--utteranceDelimiter", Arg.Set_string utteranceDelimiter, " Utterance delimiter"; 
					"-ud", Arg.Set_string utteranceDelimiter, " Short for --utteranceDelimiter"; 
					"--windowSize", Arg.Set_int windowSize, " Window size for n-grams";
					"-ws", Arg.Set_int windowSize, " Short for --windowSize";
					"--featureChart", Arg.Set_string featureFile, " Feature chart file";
					"-fc", Arg.Set_string featureFile, " Short for --featureChart";					
					"--badScore", Arg.Set_float badScore, " Score assigned when word length is less than window size";
					"-bs", Arg.Set_float badScore, " Short for --badScore";
					"--initialCount", Arg.Set_float initialNgramCount, " Count assigned to phonotactic n-grams before they are seen (default = 0.0000001)";
					"-ic", Arg.Set_float initialNgramCount, " Short for --initialCount";
					"--lineNumbers", Arg.Set displayLineNumbers, " Display line numbers before each segmented utterance";
					"-ln", Arg.Set displayLineNumbers, " Short for --lineNumbers";
					"--printUtteranceDelimiter", Arg.Set printUtteranceDelimiter, " Print utterance delimiter at the end of each utterance";
					"-pu", Arg.Set printUtteranceDelimiter, " Short for --printUtteranceDelimiter";
					"--lexiconOut", Arg.Set_string lexiconOut, " File to dump final lexicon to";
					"-lo", Arg.Set_string lexiconOut, " Short for --lexiconOut";
					"--ngramsOut", Arg.Set_string phonemeCountsOut, " File to dump final n-gram counts to";
					"-no", Arg.Set_string phonemeCountsOut, " Short for --ngramsOut";
					"--jointProbability", Arg.Set jointProb, " Use joint probabilities instead of conditional";
					"-jp", Arg.Set jointProb, " Short for --jointProbability";
					"--tokenPhonotactics", Arg.Set tokenPhonotactics, " Update phoneme n-gram counts once per word occurrence, instead of per word type.";
					"-tp", Arg.Set tokenPhonotactics, " Short for --tokenPhonotactics"]
										
let usage = Sys.executable_name ^ " [-options] CORPUS";;
Arg.parse arg_spec_list	process_anon_args usage;;

(* Setup initial value for utteranceDelimiter in the lexicon *)
Hashtbl.add lexicon !utteranceDelimiter 0;;



