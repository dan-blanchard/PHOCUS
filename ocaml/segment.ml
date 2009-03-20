(* Dan Blanchard
   Segmentation Framework *)

(*	NOTES:
		-	Calculating phoneme ngrams by increasing counts of ngrams in hypothetical words leads to segmentation of second utterance with Venkataraman.
		-	FeatureNgramCue has not been updated to account for fixes in PhonemeNgramCue.
		-	Consider functor implementation of ngram cues to clean up redundant code.
		-	Must figure out why framework MBDP and non-framework MBDP differ.
	*)
open Pcre
open Printf
open ExtList
open ExtString
open ExtArray

let svnRevision = "UNKNOWN"
let printUtteranceDelimiter = ref false
let displayLineNumbers = ref false
let featureFile = ref ""
let badScore =  ref 0.0
let initialNgramCount = ref 0.0000001
let wordDelimiter = ref " "
let utteranceDelimiter = ref "$"
let corpus = ref ""
let sentenceList = ref []
let lexiconOut = ref ""
let phonemeCountsOut = ref ""
let windowSize = ref 1
let jointProb = ref false
let smooth = ref false
let tokenPhonotactics = ref false
let totalWords = ref 0
let mbdp = ref false
let countProposedNgrams = ref false
let utteranceLimit = ref 0
let lexicon = Hashtbl.create 10000

let removeSpacesPattern = regexp "((\\s)|(\\.))+"

(* Process command-line arguments - this code must precede module definitions in order for their variables to get initialized correctly *)
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
					"--utteranceLimit", Arg.Set_int utteranceLimit, " Number of utterances in input corpus to process. (default = 0, which examines all)";
					"-ul", Arg.Set_int utteranceLimit, " Short for --utteranceLimit";
					"--printUtteranceDelimiter", Arg.Set printUtteranceDelimiter, " Print utterance delimiter at the end of each utterance";
					"-pu", Arg.Set printUtteranceDelimiter, " Short for --printUtteranceDelimiter";
					"--lexiconOut", Arg.Set_string lexiconOut, " File to dump final lexicon to";
					"-lo", Arg.Set_string lexiconOut, " Short for --lexiconOut";
					"--ngramsOut", Arg.Set_string phonemeCountsOut, " File to dump final n-gram counts to";
					"-no", Arg.Set_string phonemeCountsOut, " Short for --ngramsOut";
					"--jointProbability", Arg.Set jointProb, " Use joint probabilities instead of conditional";
					"-jp", Arg.Set jointProb, " Short for --jointProbability";
					"--tokenPhonotactics", Arg.Set tokenPhonotactics, " Update phoneme n-gram counts once per word occurrence, instead of per word type.";
					"-tp", Arg.Set tokenPhonotactics, " Short for --tokenPhonotactics";
					"--hypotheticalPhonotactics", Arg.Set countProposedNgrams, " When evaluating hypothetical words' well-formedness, increment counts of all n-grams within proposed word. (Default = false)";
					"-hp", Arg.Set countProposedNgrams, " Short for --hypotheticalPhonotactics";
					"--MBDP", Arg.Set mbdp, " Use MBDP-1 (Brent 1999) phoneme and word scores functions.  Should also enable --hypotheticalPhonotactics for true MBDP-1.";
					"-mb", Arg.Set mbdp, " Short for --MBDP-1"]

let usage = Sys.executable_name ^ " [-options] CORPUS";;
Arg.parse arg_spec_list	process_anon_args usage;;

let hash_fprint_float file = Hashtbl.iter (fun key data -> fprintf file "%s\t%g\n" key data);;
let hash_fprint_int file = Hashtbl.iter (fun key data -> fprintf file "%s\t%d\n" key data);;

(* Framework components: search algorithm, evidence/cues, method for combining evidence, method for updating stats, corpus processor *)

module type CUE =
sig
	(* Returns negative log probability that proposed word is a word.
	 * Takes function that determines how to combine scores in the case of sub-scores as argument. *)
	val eval_word : string -> (float -> float -> float) -> float
	
	(* Initializes any statistics to their default values, if necessary *)
	val initialize : float -> unit
	
	(* Takes a word and updates any evidence the cue keeps track of based on another occurrence of this word. *)
	val update_evidence : string -> unit
	
	(* Dump the table associated with this cue to a file. *)
	val dump : string -> unit
end

module FamiliarWordCue : CUE = 
struct
	(* Returns negative log of frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		if (Hashtbl.mem lexicon word) then
			let wordCountFloat = float (Hashtbl.find lexicon word) in
			let totalWordsFloat = float !totalWords in 
			if (not !mbdp) then
				let wordTypesFloat = float ((Hashtbl.length lexicon) - 1) in (* Subtract one for initial utterance delimiter addition *)
					-.(log (wordCountFloat /. (totalWordsFloat +. wordTypesFloat)))
			else
				-.(log (((wordCountFloat +. 1.0) /. (totalWordsFloat +. 1.0)) *. (((wordCountFloat) /. (wordCountFloat +. 1.0)) ** 2.0)))
		else
			-.(log !badScore)
	
	let initialize initialCount = ()
	
	let dump dumpFile = ()
	
	let update_evidence (newWord:string) = 
		totalWords := !totalWords + 1;
		if Hashtbl.mem lexicon newWord then
			Hashtbl.replace lexicon newWord ((Hashtbl.find lexicon newWord) + 1)
		else
			Hashtbl.add lexicon newWord 1;;		
end

module PhonemeNgramCue : CUE = struct
	let sixOverPiSquared = 6.0 /. (3.1415926536 ** 2.0)
	
	let ngramCountsArray = Array.init (!windowSize) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))))
	let totalNgramsArray = Array.init (!windowSize) (fun a -> 0.0)
	let typesWithCountArray = Array.init 3 (fun a -> 0)
	let ngramList = List.init !windowSize (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	
	(* Takes a list of strings and returns all permutations of them of length (n + 1) *)		
	let rec permutations permList n = 
		if n = 0 then
			permList
		else
			List.fold_left
				(fun resultList item -> 
					let concatList = List.map (fun x -> x ^ item) permList in
					resultList @ concatList
				) 
				[]
				(permutations permList (n - 1));;
		
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount =
		let phonemeList = !wordDelimiter :: (Std.input_list (Unix.open_process_in ("gsed -r 's/(.)/\\1\\n/g' " ^ !corpus ^ " | gsed '/^$/d' | sort | uniq"))) in
		List.iter 
			(fun currentWindowSizeMinusOne ->
				let phonemePermutationList = permutations phonemeList currentWindowSizeMinusOne in
				List.iter
					(fun ngram -> 
						Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram initialCount;
					)
					phonemePermutationList;
				Array.set totalNgramsArray currentWindowSizeMinusOne ((float (List.length phonemePermutationList)) *. initialCount)
			)
			ngramList
	
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

	(* The implementation of this function is NOT done yet. *)
	let prob_ngram_kneser_ney ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray = 
		let prefix = String.sub ngram 0 n in
		(* let d = discount n in *)
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
		let wordTypesFloat = float (Hashtbl.length lexicon) in (* Don't need to add one for MBDP because the initial addition of the utterance delimiter makes this one higher *)
		let totalWordsFloat = float (!totalWords + (if !mbdp then 1 else 0)) in
		let score = ref 0.0 in
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
								if Hashtbl.mem wordNgramCountsArray.(currentWindowSizeMinusOne) ngram then
									Hashtbl.replace wordNgramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find wordNgramCountsArray.(currentWindowSizeMinusOne) ngram) +. 1.0)
								else if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
									Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) +. 1.0)
								else
									Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram 1.0;
							)
							ngramFirstCharList;
						Array.set wordTotalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. (float ngramFirstCharListLength))
					)
					ngramList;
				let currentTotalNgramsArray = (if !countProposedNgrams then wordTotalNgramsArray else totalNgramsArray) in
				let currentNgramCountsArray = (if !countProposedNgrams then wordNgramCountsArray else ngramCountsArray) in								
				if (not !mbdp) then
					score := -.(log (wordTypesFloat /. (wordTypesFloat +. totalWordsFloat)))
				else
					score := 0.0;
				score := !score +. (if !windowSize > 1 then 
											-.(log 1.0)
										else
											-.(log (1.0 /. (1.0 -. ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) /. currentTotalNgramsArray.(0))))));
				(* printf "basePhonemeScore = %e\twordDelimiterCount = %e\twordtotal = %e\n" !score (Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) currentTotalNgramsArray.(0);  *)
				List.iter (* Get ngram scores *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar !windowSize in
						let ngramScore = (-. (log (prob_ngram ngram (!windowSize - 1) currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray))) in
						(* printf "\tNgram score for %s = %e\n" ngram ngramScore; *)
						score := (combine !score ngramScore)
					)
					(List.init ((String.length wordWithBoundary) - (!windowSize - 1)) (fun a -> a));
				if (not !mbdp) then
					!score
				else
					begin
						let adjustment = -. (log (sixOverPiSquared *. (wordTypesFloat /. (totalWordsFloat)))) -. (log (((wordTypesFloat -. 1.0) /. wordTypesFloat) ** 2.0)) in
						(* printf "Score adjustment = %e\n" adjustment;
						printf "Raw phoneme score = %e\n" !score; *)
						!score +. adjustment
					end
			end
		
	let update_evidence (newWord:string) = 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
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
	
	(* Dump the table associated with this cue to a file. *)
	let dump dumpFile = 
		let oc = open_out dumpFile in
		List.iter
			(fun currentWindowSizeMinusOne ->
				fprintf oc "CURRENT WINDOW SIZE: %d\n" (currentWindowSizeMinusOne + 1);
				hash_fprint_float oc ngramCountsArray.(currentWindowSizeMinusOne);
			)
			ngramList;
		close_out oc
end

module FeatureNgramCue : CUE = struct
	let ngramCountsArray = Array.init (!windowSize) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))))
	let totalNgramsArray = Array.init (!windowSize) (fun a -> 0.0)
	let typesWithCountArray = Array.init 3 (fun a -> 0)
	let cartesianProductCache = Hashtbl.create 10000
	let ngramList = List.init !windowSize (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	module StringSet = Set.Make(String)
	
	let initialize initialCount = ()
	
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
			let temp = (Hashtbl.find wordNgramCountsArray.(n) ngram) /. wordTotalNgramsArray.(n) in
			(* printf "ngram score for %s = %e\n" ngram temp; *)
			temp
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

	(* The implementation of this function is NOT done yet. *)
	let prob_ngram_kneser_ney ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray = 
		let prefix = String.sub ngram 0 n in
		(* let d = discount n in *)
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
				List.iter (* Get feature n-gram counts of all size *)
					(fun currentWindowSizeMinusOne ->						
						let firstCharListForBundles = Array.init (String.length wordWithBoundary) (fun a -> a) in
						let firstCharList = List.init ((String.length wordWithBoundary) - currentWindowSizeMinusOne) (fun a -> a) in
						let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
												(fun firstChar ->
													let phoneme = String.sub wordWithBoundary firstChar 1 in
													Featurechart.features_for_phone phoneme
												) 
												firstCharListForBundles 
						in						
						List.iter
							(fun firstChar ->
								let ngramFeatures = Array.sub wordFeatures firstChar (currentWindowSizeMinusOne + 1) in
								let lastCharList = List.init currentWindowSizeMinusOne (fun a -> (a + 1)) in
								let ngramFeatureSet = List.fold_left
														(fun currentProduct lastChar ->
															let subWord = String.sub wordWithBoundary firstChar (lastChar + 1) in
															if Hashtbl.mem cartesianProductCache subWord then															
																begin
																	Hashtbl.find cartesianProductCache subWord
																end
															else
																begin															
																	let newProduct = StringSet.fold 
																						(fun currentFeature currentSet->
																							StringSet.union 
																								currentSet
																								(StringSet.fold
																									(fun otherFeature otherSet ->
																										StringSet.add (currentFeature ^ otherFeature) otherSet
																									)
																									ngramFeatures.(lastChar)
																									StringSet.empty)
																						)
																						currentProduct 
																						StringSet.empty
																	in
																	Hashtbl.add cartesianProductCache subWord newProduct;
																	newProduct
																end
														)
														ngramFeatures.(0)
														lastCharList;
								in
								StringSet.iter
									(fun featureGram ->
										if Hashtbl.mem wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram then
											Hashtbl.replace wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram) +. 1.0)
										else if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) featureGram then
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) featureGram) +. 1.0)
										else
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram 1.0;
										Array.set wordTotalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. 1.0)
									)
									ngramFeatureSet;
							)
							firstCharList
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
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			let wordWithBoundary = (if !windowSize > 1 then 
										!wordDelimiter ^ newWord ^ !wordDelimiter 
									else 
										newWord ^ !wordDelimiter) in
			let wordWindow = (if (String.length wordWithBoundary) < !windowSize then
									String.length wordWithBoundary
							else
								!windowSize) in
			let wordNgramList = List.init wordWindow (fun a -> a) in
			List.iter (* Get feature n-gram counts of all size *)
				(fun currentWindowSizeMinusOne ->											
					let firstCharListForBundles = Array.init (String.length wordWithBoundary) (fun a -> a) in
					let firstCharList = List.init ((String.length wordWithBoundary) - currentWindowSizeMinusOne) (fun a -> a) in
					let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
											(fun firstChar ->
												let phoneme = String.sub wordWithBoundary firstChar 1 in												
												Featurechart.features_for_phone phoneme
											) 
											firstCharListForBundles 
					in
					List.iter
						(fun firstChar ->							
							let ngramFeatures = Array.sub wordFeatures firstChar (currentWindowSizeMinusOne + 1) in
							let lastCharList = List.init currentWindowSizeMinusOne (fun a -> (a + 1)) in
							let ngramFeatureSet = List.fold_left
													(fun currentProduct lastChar ->
														let subWord = String.sub wordWithBoundary firstChar (lastChar + 1) in
														if Hashtbl.mem cartesianProductCache subWord then
															Hashtbl.find cartesianProductCache subWord
														else
															begin															
																let newProduct = StringSet.fold 
																					(fun currentFeature currentSet->
																						StringSet.union 
																							currentSet
																							(StringSet.fold
																								(fun otherFeature otherSet ->
																									StringSet.add (currentFeature ^ otherFeature) otherSet
																								)
																							StringSet.empty
																							ngramFeatures.(lastChar))
																					)
																					StringSet.empty
																					currentProduct 
																in
																Hashtbl.add cartesianProductCache subWord newProduct;
																newProduct
															end
													)
													StringSet.empty
													lastCharList;
							in
							StringSet.iter
								(fun featureGram ->
									if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) featureGram then
										Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) featureGram) +. 1.0)
									else
										Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) featureGram 1.0;
									Array.set totalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. 1.0)
								)
								ngramFeatureSet;							
						)
						firstCharList;
				)
				wordNgramList
	
	(* Dump the table associated with this cue to a file. *)
	let dump dumpFile = 
		let oc = open_out dumpFile in
		List.iter
			(fun currentWindowSizeMinusOne ->
				fprintf oc "CURRENT WINDOW SIZE: %d\n" (currentWindowSizeMinusOne + 1);
				hash_fprint_float oc ngramCountsArray.(currentWindowSizeMinusOne);
			)
			ngramList;
		close_out oc
end;;


(*****BEGIN MAIN PROGRAM******)


(* Read corpus file, if specified, otherwise read from stdin *)
if !corpus <> "" then
	let ic = open_in !corpus in
	try
		sentenceList := Std.input_list ic;
		close_in ic;
		(* Initialize phoneme counts, if not MBDP *)
		if (not !mbdp) then
			(PhonemeNgramCue.initialize !initialNgramCount)
	with e ->
		close_in_noerr ic;
		raise e
else
	begin
		sentenceList := Std.input_list stdin;
		close_in stdin
	end;;

(* Read feature file, if specifed *)
if !featureFile <> "" then
	Featurechart.read_feature_file !featureFile;;

(* Setup initial value for utteranceDelimiter in the lexicon *)
Hashtbl.add lexicon !utteranceDelimiter 0;;

(* Function lists store which functions will be called during word evaluation and updating *)
let wordEvalFunctions = [PhonemeNgramCue.eval_word; FamiliarWordCue.eval_word];;


(* Updates lexicon with newly segmented words, and prints out segmented utterance *)
let rec lexicon_updater segmentation sentence updateFunctions =
	if (List.length segmentation) > 1 then
		begin
			let startChar = List.nth segmentation 0 in
			let endChar = List.nth segmentation 1 in
			let newWord = String.sub sentence startChar (endChar - startChar) in
			List.iter (fun updateFunc -> (updateFunc newWord)) updateFunctions; (* Calls all of the update functions in the list *)
			printf "%s" (newWord ^ !wordDelimiter);
			lexicon_updater (List.tl segmentation) sentence updateFunctions
		end
	else
		();;

let default_evidence_combiner word =
	let familiarScore = FamiliarWordCue.eval_word word (+.) in
	let phonemeScore = PhonemeNgramCue.eval_word word (+.) in
	(* printf "Familiar score for %s = %e\nPhoneme score for %s = %e\n\n" word familiarScore word phonemeScore; *)
	if (Hashtbl.mem lexicon word) then
		familiarScore
	else
		phonemeScore;;

let eval_word = default_evidence_combiner;;
		
let rec mbdp_inner subUtterance firstChar lastChar bestList =
	if firstChar <= lastChar then
		begin
			let newSubUtterance = String.sub subUtterance firstChar ((lastChar + 1) - firstChar) in
			let wordScore = eval_word newSubUtterance in
			let oldBestProduct = fst (bestList.(firstChar - 1)) in
			let lastCharBestProduct = fst (bestList.(lastChar)) in
			let scoreProduct = wordScore +. oldBestProduct in
			if scoreProduct < lastCharBestProduct then
				mbdp_inner subUtterance (firstChar + 1) lastChar (Array.concat [(Array.sub bestList 0 lastChar); [|(scoreProduct, firstChar)|]; (Array.sub bestList (lastChar + 1) ((Array.length bestList) - (lastChar + 1)))])
			else	
				mbdp_inner subUtterance (firstChar + 1) lastChar bestList
		end
	else
		bestList;;

let mbdp_outer sentence =
	let lastCharList = Array.init (String.length sentence) (fun a -> a) in
	let bestList = Array.fold_left
		(fun oldBestList lastChar ->
			(* printf "LastChar: %i\tString length: %i\n" lastChar (String.length sentence); *)
			let subUtterance = String.sub sentence 0 (lastChar + 1) in
			let newBestList = Array.append oldBestList [|((eval_word subUtterance), 0)|] in
			mbdp_inner subUtterance 1 lastChar newBestList
		)
		[||]
		lastCharList
	in
	(* List.iter (fun (x, y) -> printf "(%e, %d)" x y) bestList; *)
	Array.map
	 	snd
		bestList;;

let evalUtterance = mbdp_outer;;

(* Finds the path through bestStartList which reveals the starts and stops of all hypothesized words in the utterance.
	Make sure you call this with firstChar set to the length of the utterance*)
let rec find_segmentation bestStartList firstChar path =
	if firstChar > 0 then
		begin
			let newFirstChar = bestStartList.(firstChar - 1) in
			find_segmentation bestStartList newFirstChar (path @ [newFirstChar])
		end
	else
		path;;

(* Loop through utterances *)
let incremental_processor utteranceList = 
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit == 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let sentence = replace ~rex:removeSpacesPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
					let bestStartList = evalUtterance sentence in
					let segmentation = (List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence] in
					if (!displayLineNumbers) then
						printf "%d: " (utteranceCount + 1);
					lexicon_updater segmentation sentence [PhonemeNgramCue.update_evidence; FamiliarWordCue.update_evidence]; 
					if (!printUtteranceDelimiter) then
						printf "%s" !utteranceDelimiter;		
					printf "\n";
					flush stdout;
					Hashtbl.replace lexicon !utteranceDelimiter ((Hashtbl.find lexicon !utteranceDelimiter) + 1)
				end
		)
		utteranceList;;

(* Learn phonotactic model from gold corpus, and then resegment without updating phonotactic model *)
let two_pass_processor utteranceList = 
	printf "\nPhonotactic Pass:\n";
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit == 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let sentence = replace ~rex:removeSpacesPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
					let segmentedChars = (String.explode segmentedSentence) in
					let boundaryChar = List.nth (String.explode !wordDelimiter) 0 in
					let boundaryIndexes = List.mapi (fun index character -> 
														if (character == boundaryChar) then 
															index 
														else 
															0) 
													segmentedChars in
					let segmentation = [0] @ (List.mapi (fun index boundary -> boundary - index) ((List.remove_all boundaryIndexes 0) @ [(String.length segmentedSentence)])) in
					lexicon_updater segmentation sentence [PhonemeNgramCue.update_evidence]; 
					printf "\n";
					flush stdout;
					Hashtbl.replace lexicon !utteranceDelimiter ((Hashtbl.find lexicon !utteranceDelimiter) + 1)
				end
		)
		utteranceList;
	Hashtbl.clear lexicon;
	Hashtbl.add lexicon !utteranceDelimiter 0;
	printf "\nSegmentation Pass:\n";
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit == 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let sentence = replace ~rex:removeSpacesPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
					let bestStartList = evalUtterance sentence in
					let segmentation = (List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence] in
					if (!displayLineNumbers) then
						printf "%d: " (utteranceCount + 1);
					lexicon_updater segmentation sentence [FamiliarWordCue.update_evidence]; 
					if (!printUtteranceDelimiter) then
						printf "%s" !utteranceDelimiter;		
					printf "\n";
					flush stdout;
					Hashtbl.replace lexicon !utteranceDelimiter ((Hashtbl.find lexicon !utteranceDelimiter) + 1)
				end
		)
		utteranceList;;


(* let sentence_processor = two_pass_processor;; *)
let sentence_processor = incremental_processor;;

(*** ACTUAL START OF PROGRAM ***)
sentence_processor !sentenceList;;

(* Dump lexicon if requested *)
if !lexiconOut <> "" then
	let oc = open_out !lexiconOut in
	hash_fprint_int oc lexicon;
	close_out oc;;

(* Dump n-gram counts if requested *)
if !phonemeCountsOut <> "" then
	PhonemeNgramCue.dump !phonemeCountsOut;;
	


