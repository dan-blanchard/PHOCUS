(*	Dan Blanchard
	Segmentation Framework *)

(*	NOTES:
		-	Consider functor implementation of ngram cues to clean up redundant code.
*)
open Pcre
open Printf
open ExtList
open ExtString
open ExtArray

module StringSet = Set.Make(String)

let e = 2.71828183
let sixOverPiSquared = 6.0 /. (3.1415926536 ** 2.0)
let printUtteranceDelimiter = ref false
let displayLineNumbers = ref false
let featureFile = ref ""
let badScore =  ref 0.0
let initialNgramCount = ref 0.0000001
let syllableDelimiter = ref "."
let wordDelimiter = ref " "
let utteranceDelimiter = ref "$"
let corpus = ref ""
let sentenceList = ref []
let lexiconOut = ref ""
let phonemeCountsOut = ref ""
let featureCountsOut = ref ""
let syllableCountsOut = ref ""
let phonemeWindow = ref 1
let syllableWindow = ref 0
let featureWindow = ref 1
let jointProb = ref false
let smooth = ref false
let tokenPhonotactics = ref false
let interactive = ref false
let totalWords = ref 0.0
let mbdp = ref false
let verbose = ref false
let countProposedNgrams = ref false
let utteranceLimit = ref 0
let lexicon = Hashtbl.create 10000
let decayFactor = ref 0.0
let supervisedFor = ref 0
let requireSyllabic = ref false

(* Process command-line arguments - this code must precede module definitions in order for their variables to get initialized correctly *)
let process_anon_args corpusFile = corpus := corpusFile
let arg_spec_list =["--badScore", Arg.Set_float badScore, " Score assigned when word length is less than window size (Default = 0.0)";
					"-bs", Arg.Set_float badScore, " Short for --badScore";
					"--decayFactor", Arg.Set_float decayFactor, " Exponent used to calculate memory decay.  (Default = 0.0, no decay)";
					"-df", Arg.Set_float decayFactor, " Short for --decayFactor";
					"--featureChart", Arg.Set_string featureFile, " Feature chart file";
					"-fc", Arg.Set_string featureFile, " Short for --featureChart";					
					"--featureNgramsOut", Arg.Set_string featureCountsOut, " File to dump final feature n-gram counts to";
					"-fn", Arg.Set_string featureCountsOut, " Short for --featureNgramsOut";
					"--featureWindow", Arg.Set_int featureWindow, " Window size for feature n-grams";
					"-fw", Arg.Set_int featureWindow, " Short for --featureWindow";
					"--hypotheticalPhonotactics", Arg.Set countProposedNgrams, " When evaluating hypothetical words' well-formedness, increment counts of all n-grams within proposed word. (Default = false)";
					"-hp", Arg.Set countProposedNgrams, " Short for --hypotheticalPhonotactics";
					"--initialCount", Arg.Set_float initialNgramCount, " Count assigned to phonotactic n-grams before they are seen (default = 0.0000001)";
					"-ic", Arg.Set_float initialNgramCount, " Short for --initialCount";
					"--interactive", Arg.Set interactive, " After reading in corpus, user can specify an utterance number to segment up to, and query scores for possible segmentations.";
					"-i", Arg.Set interactive, " Short for --interactive";
					"--jointProbability", Arg.Set jointProb, " Use joint probabilities instead of conditional";
					"-jp", Arg.Set jointProb, " Short for --jointProbability";
					"--lexiconOut", Arg.Set_string lexiconOut, " File to dump final lexicon to";
					"-lo", Arg.Set_string lexiconOut, " Short for --lexiconOut";
					"--lineNumbers", Arg.Set displayLineNumbers, " Display line numbers before each segmented utterance";
					"-ln", Arg.Set displayLineNumbers, " Short for --lineNumbers";
					"--MBDP", Arg.Set mbdp, " Use MBDP-1 (Brent 1999) phoneme and word scores functions.  Should also enable --hypotheticalPhonotactics for true MBDP-1.";
					"-mb", Arg.Set mbdp, " Short for --MBDP-1";
					"--phonemeNgramsOut", Arg.Set_string phonemeCountsOut, " File to dump final phoneme n-gram counts to";
					"-pn", Arg.Set_string phonemeCountsOut, " Short for --phonemeNgramsOut";
					"--phonemeWindow", Arg.Set_int phonemeWindow, " Window size for phoneme n-grams";
					"-pw", Arg.Set_int phonemeWindow, " Short for --phonemeWindow";
					"--printUtteranceDelimiter", Arg.Set printUtteranceDelimiter, " Print utterance delimiter at the end of each utterance";
					"-pu", Arg.Set printUtteranceDelimiter, " Short for --printUtteranceDelimiter";
					"--requireSyllabic", Arg.Set requireSyllabic, " Require each proposed word to contain at least one syllabic sound.  (Requires --featureChart that includes 'syllabic' as feature)";
					"-rs", Arg.Set requireSyllabic, " Short for --requireSyllabic";
					"--supervisedFor", Arg.Set_int supervisedFor, " Number of utterances to use given word-boundaries for.  (Default = 0, unsupervised learner)";
					"-sf", Arg.Set_int supervisedFor, " Short for --supervisedFor";
					"--syllableNgramsOut", Arg.Set_string syllableCountsOut, " File to dump final syllable n-gram counts to";
					"-sn", Arg.Set_string syllableCountsOut, " Short for --syllableNgramsOut";
					"--syllableWindow", Arg.Set_int syllableWindow, " Window size for syllable n-grams";
					"-sw", Arg.Set_int syllableWindow, " Short for --syllableWindow";
					"--tokenPhonotactics", Arg.Set tokenPhonotactics, " Update phoneme n-gram counts once per word occurrence, instead of per word type.";
					"-tp", Arg.Set tokenPhonotactics, " Short for --tokenPhonotactics";
					"--utteranceDelimiter", Arg.Set_string utteranceDelimiter, " Utterance delimiter"; 
					"-ud", Arg.Set_string utteranceDelimiter, " Short for --utteranceDelimiter"; 
					"--utteranceLimit", Arg.Set_int utteranceLimit, " Number of utterances in input corpus to process. (default = 0, which examines all)";
					"-ul", Arg.Set_int utteranceLimit, " Short for --utteranceLimit";
					"--verbose", Arg.Set verbose, " Print out scores for each possible segmentation of each utterance.";
					"-v", Arg.Set verbose, " Short for --verbose";
					"--wordDelimiter", Arg.Set_string wordDelimiter, " Word delimiter";
					"-wd", Arg.Set_string wordDelimiter, " Short for --wordDelimiter"]

let usage = Sys.executable_name ^ " [-options] CORPUS";;
Arg.parse (Arg.align arg_spec_list) process_anon_args usage;;

(* Read feature file, if specifed *)
if !featureFile <> "" then
	Featurechart.read_feature_file !featureFile;;

let hash_fprint_float file = Hashtbl.iter (fun key data -> fprintf file "%s\t%g\n" key data);;
let hash_fprint_int file = Hashtbl.iter (fun key data -> fprintf file "%s\t%d\n" key data);;

let removeDelimitersPattern = regexp (!wordDelimiter ^ "+")

let (diphthongs, vowels) = if (!featureFile <> "" ) then 
								StringSet.partition (fun phone -> (String.length phone) > 1) (Featurechart.phones_for_features (StringSet.singleton "+syllabic"))
							else
								(StringSet.empty, StringSet.empty)
let diphthongPattern = regexp (if (!featureFile <> "" ) then
									if (StringSet.is_empty diphthongs) then
										"(9I)|(OI)|(9U)"
									else
										(StringSet.fold 
											(fun diphthong currentString -> 
												(if currentString <> "" then "|" else "") ^ "(" ^ diphthong ^ ")"
											) 
											diphthongs 
											""
										)
								else 
									"(9I)|(OI)|(9U)") 
let vowelPattern = regexp ("[" ^ (if (!featureFile <> "" ) then 
										(StringSet.fold (^) vowels "") 
								else "&69AEIOUaeiouR~ML") ^ "]")
let noVowelsPattern = regexp ("[^" ^ (if (!featureFile <> "" ) then 
										(StringSet.fold (^) vowels "") 
									else "&69AEIOUaeiouR~ML") ^ "]")
let onsetPattern = regexp "[^0-9>](0+|1+|2+|3+|4+|5+|6+|7+|8+|9+)"
let codaPattern = regexp "(0+|1+|2+|3+|4+|5+|6+|7+|8+|9+)[^0-9<]"
let notNumPattern = regexp "[^0-9]"
let onsetReplace = subst "<$1"
let codaReplace = subst "$1>"
let onsetCleanupPattern = regexp "<([0-9])"
let codaCleanupPattern = regexp "([0-9])>"
let onsetCleanupReplace = subst "$1$1"
let codaCleanupReplace = subst "$1$1"

(* Insert syllable boundaries into word by finding nucleus, then adding possible onsets and codas (in that order). 
   If word does not contain any syllabic elements, returns "" *)
let syllabify (word:string) = 
	if (pmatch ~rex:vowelPattern word) then
		begin
			let syllCount = ref 0 in
			let replace_with_count matched = (syllCount := (!syllCount + 1) mod 10; String.init (String.length matched) (fun i -> char_of_int (!syllCount + 48))) in (* need + 48 because char_of_int looks up by ASCII code *)
			let workingWord = ref word in
			workingWord := replace ~rex:diphthongPattern ~templ:"--" !workingWord;
			workingWord := substitute ~rex:vowelPattern ~subst:replace_with_count !workingWord;
			workingWord := substitute ~pat:"--" ~subst:replace_with_count !workingWord;
			while pmatch ~rex:notNumPattern !workingWord do
				workingWord := replace ~rex:onsetPattern ~itempl:onsetReplace !workingWord;
				workingWord := replace ~rex:codaPattern ~itempl:codaReplace !workingWord;
				workingWord := replace ~rex:onsetCleanupPattern ~itempl:onsetCleanupReplace !workingWord;
				workingWord := replace ~rex:codaCleanupPattern ~itempl:codaCleanupReplace !workingWord;
			done;
			fst (List.fold_left2
					(fun (currentResult, lastSyllable) workingChar wordChar -> 
						((currentResult ^ (if workingChar <> lastSyllable then !syllableDelimiter else "") ^ (String.of_char wordChar)), workingChar)
					)
					("", '1')
					(String.explode !workingWord) 
					(String.explode word))
		end
	else
		""

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


module type CUE =
sig
	(* Returns negative log probability that proposed word is a word.
	 * Takes function that determines how to combine scores in the case of sub-scores as argument. *)
	val eval_word : string -> (float -> float -> float) -> float
	
	(* Initializes any statistics to their default values, if necessary *)
	val initialize : float -> unit
	
	(* Takes a word and updates any evidence the cue keeps track of based on another occurrence of this word. *)
	val update_evidence : string -> float -> unit
	
	(* Dump the table associated with this cue to a file. *)
	val dump : string -> unit
	
	(* Check if we should back-off or use this cue*)
	val use_score : string -> bool
end

module FamiliarWordCue : CUE = 
struct
	let lastSeen = Hashtbl.create 10000
	
	(* Returns negative log of frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		if (Hashtbl.mem lexicon word) then
			let wordCountFloat = (Hashtbl.find lexicon word) in
			let totalWordsFloat = !totalWords in 
			let rawScore = 
				(if (not !mbdp) then
					let wordTypesFloat = float ((Hashtbl.length lexicon) - 1) in (* Subtract one for initial utterance delimiter addition *)
						(wordCountFloat /. (totalWordsFloat +. wordTypesFloat))
				else
					(((wordCountFloat +. 1.0) /. (totalWordsFloat +. 1.0)) *. (((wordCountFloat) /. (wordCountFloat +. 1.0)) ** 2.0)))
			in
			-.(log (rawScore *. (((Hashtbl.find lexicon !utteranceDelimiter) +. 1.0) -. (Hashtbl.find lastSeen word)) ** (-. !decayFactor)))
		else
			-.(log !badScore)
	
	let initialize initialCount = ()
	
	let dump dumpFile = ()
	
	let use_score (word:string) = Hashtbl.mem lexicon word
	
	let update_evidence (newWord:string) (incrementAmount:float)= 
		totalWords := !totalWords +. incrementAmount;
		if Hashtbl.mem lexicon newWord then
			Hashtbl.replace lexicon newWord ((Hashtbl.find lexicon newWord) +. incrementAmount)
		else
			Hashtbl.add lexicon newWord incrementAmount;
		Hashtbl.replace lastSeen newWord (Hashtbl.find lexicon !utteranceDelimiter);;		
end

module type NgramProbsSig = 
sig
	val prob_ngram : string -> string -> int -> (string, float) Hashtbl.t array -> float array -> int array -> (string, float) Hashtbl.t array -> float
end

module NgramProbs : NgramProbsSig =
struct
	(* Calculates D_n for Modified Kneser-Ney Smoothing*)
	let rec discount n wordTypesWithCountArray = 
		if (n = 0) then
			wordTypesWithCountArray.(0) / (wordTypesWithCountArray.(0) + (2 * wordTypesWithCountArray.(1)))
		else if (n < 3) then
			n - ((n + 1) * (discount 0 wordTypesWithCountArray) * (wordTypesWithCountArray.(n) / (wordTypesWithCountArray.(n - 1))))
		else
			3 - (4 * (discount 0 wordTypesWithCountArray) * (wordTypesWithCountArray.(4) / (wordTypesWithCountArray.(3))));;

	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram_conditional prefix ngram n wordNgramCountsArray wordTotalNgramsArray ngramCountsArray =
		if (n = 0) then
			if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
				-.((log (Hashtbl.find wordNgramCountsArray.(n) ngram)) -. (log wordTotalNgramsArray.(n)))
			else
				!badScore
		else
			begin
				if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							-.((log (Hashtbl.find wordNgramCountsArray.(n) ngram)) -. (log (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)))
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							-.((log (Hashtbl.find wordNgramCountsArray.(n) ngram)) -. (log (Hashtbl.find ngramCountsArray.(n - 1) prefix)))
						else
							!badScore
					end
				else
					if (Hashtbl.mem ngramCountsArray.(n) ngram) then
						begin					
							if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
								-.((log (Hashtbl.find ngramCountsArray.(n) ngram)) -. (log (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)))
							else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
								-.((log (Hashtbl.find ngramCountsArray.(n) ngram)) -. (log (Hashtbl.find ngramCountsArray.(n - 1) prefix)))
							else
								!badScore
						end
					else
						!badScore
			end;;

	(* The implementation of this function is NOT done yet. *)
	let prob_ngram_kneser_ney prefix ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray ngramCountsArray = 
		(* let d = discount n in *)
		if (n = 0) then
			-.((log (Hashtbl.find wordNgramCountsArray.(n) ngram)) -. (log wordTotalNgramsArray.(n)))
		else
			begin
				if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							-.((log (Hashtbl.find wordNgramCountsArray.(n) ngram)) -. (log (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)))
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							-.((log (Hashtbl.find wordNgramCountsArray.(n) ngram)) -. (log (Hashtbl.find ngramCountsArray.(n - 1) prefix)))
						else
							!badScore
					end
				else 
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							-.((log (Hashtbl.find ngramCountsArray.(n) ngram)) -. (log (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)))
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							-.((log (Hashtbl.find ngramCountsArray.(n) ngram)) -. (log (Hashtbl.find ngramCountsArray.(n - 1) prefix)))
						else
							!badScore
					end								
			end;;

	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram_joint prefix ngram n wordNgramCountsArray wordTotalNgramsArray =
		-.((log (Hashtbl.find wordNgramCountsArray.(n) ngram)) -. (log wordTotalNgramsArray.(n)));;

	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram prefix ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray ngramCountsArray = 
		match (!jointProb, !smooth) with 
			(false, true)  -> prob_ngram_kneser_ney prefix ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray ngramCountsArray
		|	(false, false) -> prob_ngram_conditional prefix ngram n wordNgramCountsArray wordTotalNgramsArray ngramCountsArray
		| 	(_,_) -> prob_ngram_joint prefix ngram n wordNgramCountsArray wordTotalNgramsArray;;
end

module SyllableNgramCue : CUE =
struct
	open NgramProbs
	let ngramCountsArray = Array.init (!syllableWindow) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))))
	let totalNgramsArray = Array.init (!syllableWindow) (fun a -> 0.0)
	let typesWithCountArray = Array.init 3 (fun a -> 0)
	let ngramList = List.init !syllableWindow (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	
	(* Check if we can even syllabify word *)	
	let use_score (word:string) = ((syllabify word) = "") 
	
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount = ()
	
	(* Collapse syllable array into string. *)
	let string_of_syllable_array syllableArray =
		Array.fold_left
			(fun a b  -> if (a <> "") then (a ^ !syllableDelimiter ^ b) else (a ^ b))
			""
			syllableArray
			
	(* Returns negative log of frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		let syllabifiedWord = syllabify word in
		let syllables = String.nsplit syllabifiedWord !syllableDelimiter in 
		if (syllabifiedWord <> "") then
			begin
				let wordNgramCountsArray = Array.init (!syllableWindow) (fun a -> Hashtbl.create 100) in
				let wordTotalNgramsArray = Array.init (!syllableWindow) (fun a -> 0.0) in
				let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in		
				let syllablesWithBoundary = Array.of_list (if !syllableWindow > 1 then 
																[!wordDelimiter] @ syllables @ [!wordDelimiter]
															else 
																syllables @ [!wordDelimiter]) in		
				let score = ref 0.0 in
				if (Array.length syllablesWithBoundary) < !syllableWindow then
					-.(log !badScore)
				else	
					begin
						List.iter (* Get n-gram counts of all size *)
							(fun currentWindowSizeMinusOne ->
								let ngramFirstSyllListLength = (Array.length syllablesWithBoundary) - currentWindowSizeMinusOne in 
								let ngramFirstSyllList = List.init ngramFirstSyllListLength (fun a -> a) in
								List.iter (* Loop through all n-grams of current size *)
									(fun firstSyll ->
										let ngram = string_of_syllable_array (Array.sub syllablesWithBoundary firstSyll (currentWindowSizeMinusOne + 1)) in
										if Hashtbl.mem wordNgramCountsArray.(currentWindowSizeMinusOne) ngram then
											Hashtbl.replace wordNgramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find wordNgramCountsArray.(currentWindowSizeMinusOne) ngram) +. 1.0)
										else if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) +. 1.0)
										else
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram 1.0;
									)
									ngramFirstSyllList;
								wordTotalNgramsArray.(currentWindowSizeMinusOne) <- (totalNgramsArray.(currentWindowSizeMinusOne) +. (float ngramFirstSyllListLength))
							)
							ngramList;
						let currentTotalNgramsArray = (if !countProposedNgrams then wordTotalNgramsArray else totalNgramsArray) in
						let currentNgramCountsArray = (if !countProposedNgrams then wordNgramCountsArray else ngramCountsArray) in
						if (currentTotalNgramsArray.(0) > 0.0) && (Hashtbl.mem currentNgramCountsArray.(0) !wordDelimiter) then
							begin
								score := 0.0;
								score := !score +. (if !syllableWindow > 1 then 
															-.(log 1.0)
														else
															(log (1.0 -. ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) /. currentTotalNgramsArray.(0)))));
								List.iter (* Get ngram scores *)
									(fun firstSyll ->
										let ngramSyllableArray = Array.sub syllablesWithBoundary firstSyll !syllableWindow in
										let ngram = string_of_syllable_array ngramSyllableArray in
										let ngramScore = prob_ngram ngramSyllableArray.(0) ngram (!syllableWindow - 1) currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray ngramCountsArray in
										(* printf "\tNgram score for %s = %F\n" ngram ngramScore; *)
										score := (combine !score ngramScore)
									)
									(List.init ((Array.length syllablesWithBoundary) - (!syllableWindow - 1)) (fun a -> a));
								!score
							end
						else
							-.(log 0.0)
					end
			end
		else
			-.(log 0.0)
		
	let update_evidence (newWord:string) (incrementAmount:float)= 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			let syllabifiedWord = syllabify newWord in
			let syllables = String.nsplit syllabifiedWord !syllableDelimiter in 
			let syllablesWithBoundary = Array.of_list (if !syllableWindow > 1 then 
															[!wordDelimiter] @ syllables @ [!wordDelimiter]
														else 
															syllables @ [!wordDelimiter]) in		
			let wordWindow = (if (Array.length syllablesWithBoundary) < !syllableWindow then
									Array.length syllablesWithBoundary
							else
								!syllableWindow) in
			let wordNgramList = List.init wordWindow (fun a -> a) in
			List.iter (* Get n-gram counts of all size *)
				(fun currentWindowSizeMinusOne ->
					let ngramFirstSyllListLength = (Array.length syllablesWithBoundary) - currentWindowSizeMinusOne in 
					let ngramFirstSyllList = List.init ngramFirstSyllListLength (fun a -> a) in
					List.iter (* Loop through all n-grams of current size *)
						(fun firstSyll ->
							let ngram = string_of_syllable_array (Array.sub syllablesWithBoundary firstSyll (currentWindowSizeMinusOne + 1)) in
							if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
								Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) +. incrementAmount)
							else
								Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram incrementAmount;
						)
						ngramFirstSyllList;
					Array.set totalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. (float ngramFirstSyllListLength))
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

module PhonemeNgramCue : CUE =
struct
	open NgramProbs
	
	let ngramCountsArray = Array.init (!phonemeWindow) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2))))) 
	let totalNgramsArray = Array.init (!phonemeWindow) (fun a -> 0.0)
	let typesWithCountArray = Array.init 3 (fun a -> 0)
	let ngramList = List.init !phonemeWindow (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount =
		let phonemeList = !wordDelimiter :: (Std.input_list (Unix.open_process_in ("gsed -r 's/(.)/\\1\\n/g' " ^ !corpus ^ " | gsed '/^$/d' | sort -u | gsed '/[ \\t]/d'"))) in
		List.iter 
			(fun currentWindowSizeMinusOne ->
				let phonemePermutationList = permutations phonemeList currentWindowSizeMinusOne in
				List.iter
					(fun ngram -> 
						Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram (initialCount *. (100.0 ** -.(float_of_int currentWindowSizeMinusOne)));
					)
					phonemePermutationList;
				Array.set totalNgramsArray currentWindowSizeMinusOne ((float (List.length phonemePermutationList)) *. initialCount)
			)
			ngramList
	
	(* Returns negative log of frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		let wordNgramCountsArray = Array.init (!phonemeWindow) (fun a -> Hashtbl.create 100) in
		let wordTotalNgramsArray = Array.init (!phonemeWindow) (fun a -> 0.0) in
		let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in		
		let wordWithBoundary = (if !phonemeWindow > 1 then 
									!wordDelimiter ^ word ^ !wordDelimiter 
								else 
									word ^ !wordDelimiter) in							
		let wordTypesFloat = float (Hashtbl.length lexicon) in (* Don't need to add one for MBDP because the initial addition of the utterance delimiter makes this one higher *)
		let totalWordsFloat = (!totalWords +. (if !mbdp then 1. else 0.0)) in
		let score = ref 0.0 in
		if (String.length wordWithBoundary) < !phonemeWindow then
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
				score := !score +. (if !phonemeWindow > 1 then 
											-.(log 1.0)
										else
											(log (1.0 -. ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) /. currentTotalNgramsArray.(0)))));
				(* printf "basePhonemeScore = %F\twordDelimiterCount = %F\twordtotal = %F\n" !score (Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) currentTotalNgramsArray.(0);  *)
				List.iter (* Get ngram scores *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar !phonemeWindow in
						let ngramScore = prob_ngram (String.sub ngram 0 (!phonemeWindow - 1)) ngram (!phonemeWindow - 1) currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray ngramCountsArray in
						(* printf "\tNgram score for %s = %F\n" ngram ngramScore; *)
						score := (combine !score ngramScore)
					)
					(List.init ((String.length wordWithBoundary) - (!phonemeWindow - 1)) (fun a -> a));
				if (not !mbdp) then
					!score
				else
					begin
						let adjustment = -. (log (sixOverPiSquared *. (wordTypesFloat /. (totalWordsFloat)))) -. (log (((wordTypesFloat -. 1.0) /. wordTypesFloat) ** 2.0)) in
						(* printf "Score adjustment = %F\n" adjustment; *)
						(* printf "Raw phoneme score = %F\n" !score; *)
						!score +. adjustment
					end
			end
		
	let update_evidence (newWord:string) (incrementAmount:float)= 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			let wordWithBoundary = (if !phonemeWindow > 1 then 
										!wordDelimiter ^ newWord ^ !wordDelimiter 
									else 
										newWord ^ !wordDelimiter) in
			let wordWindow = (if (String.length wordWithBoundary) < !phonemeWindow then
									String.length wordWithBoundary
							else
								!phonemeWindow) in
			let wordNgramList = List.init wordWindow (fun a -> a) in
			List.iter (* Get n-gram counts of all size *)
				(fun currentWindowSizeMinusOne ->
					let ngramFirstCharListLength = (String.length wordWithBoundary) - currentWindowSizeMinusOne in 
					let ngramFirstCharList = List.init ngramFirstCharListLength (fun a -> a) in
					List.iter (* Loop through all n-grams of current size *)
						(fun firstChar ->
							let ngram = String.sub wordWithBoundary firstChar (currentWindowSizeMinusOne + 1) in
							if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
								Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) +. incrementAmount)
							else
								Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram incrementAmount;
						)
						ngramFirstCharList;
					Array.set totalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. (float ngramFirstCharListLength))
				)
				wordNgramList
	
	let use_score (word:string) = true
	
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

module FeatureNgramCue : CUE =
struct
	open NgramProbs
	let ngramCountsArray = Array.init (!featureWindow) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))))
	let totalNgramsArray = Array.init (!featureWindow) (fun a -> 0.0)
	let typesWithCountArray = Array.init 3 (fun a -> 0)
	let cartesianProductCache = Hashtbl.create 10000
	let ngramList = List.init !featureWindow (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount =
		let featureList = Std.input_list (Unix.open_process_in ("head -n 1 " ^ !featureFile ^ " | gsed -r 's/^\\t//' | tr '\\t' '\\n'")) in
		let featureValueList = (List.map (fun a -> "+" ^ a) featureList) @ (List.map (fun a -> "-" ^ a) featureList) in
		List.iter 
			(fun currentWindowSizeMinusOne ->
				let featurePermutationList = permutations featureValueList currentWindowSizeMinusOne in
				List.iter
					(fun ngram -> 
						Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram (initialCount *. (100.0 ** -.(float_of_int currentWindowSizeMinusOne)));
					)
					featurePermutationList;
				Array.set totalNgramsArray currentWindowSizeMinusOne ((float (List.length featurePermutationList)) *. initialCount)
			)
			ngramList
		
	let eval_word (word:string) combine = 
		let wordNgramCountsArray = Array.init (!featureWindow) (fun a -> Hashtbl.create 100) in
		let wordTotalNgramsArray = Array.init (!featureWindow) (fun a -> 0.0) in
		let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in		
		let wordWithBoundary = (if !featureWindow > 1 then 
									!wordDelimiter ^ word ^ !wordDelimiter 
								else 
									word ^ !wordDelimiter) in							
		let wordTypesFloat = float (Hashtbl.length lexicon) in (* Don't need to add one for MBDP because the initial addition of the utterance delimiter makes this one higher *)
		let totalWordsFloat = (!totalWords +. (if !mbdp then 1. else 0.0)) in
		let score = ref 0.0 in
		if (String.length wordWithBoundary) < !featureWindow then
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
														lastCharList
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
									ngramFeatureSet
							)
							firstCharList
					)
					ngramList;
				let currentTotalNgramsArray = (if !countProposedNgrams then wordTotalNgramsArray else totalNgramsArray) in
				let currentNgramCountsArray = (if !countProposedNgrams then wordNgramCountsArray else ngramCountsArray) in								
				if (not !mbdp) then
					score := -.(log (wordTypesFloat /. (wordTypesFloat +. totalWordsFloat)))
				else
					score := 0.0;
				score := !score +. (if !phonemeWindow > 1 then 
											-.(log 1.0)
										else
											(log (1.0 -. ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) /. currentTotalNgramsArray.(0)))));
				List.iter (* Get ngram scores *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar !featureWindow in
						let ngramScore = prob_ngram (String.sub ngram 0 (!featureWindow - 1)) ngram (!featureWindow - 1) currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray ngramCountsArray in
						(* printf "\tNgram score for %s = %F\n" ngram ngramScore; *)
						score := (combine !score ngramScore)
					)
					(List.init ((String.length wordWithBoundary) - (!featureWindow - 1)) (fun a -> a));
				if (not !mbdp) then
					!score
				else
					begin
						let adjustment = -. (log (sixOverPiSquared *. (wordTypesFloat /. (totalWordsFloat)))) -. (log (((wordTypesFloat -. 1.0) /. wordTypesFloat) ** 2.0)) in
						(* printf "Score adjustment = %F\n" adjustment; *)
						(* printf "Raw phoneme score = %F\n" !score; *)
						!score +. adjustment
					end
			end
	
	let update_evidence (newWord:string) (incrementAmount:float)= 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			let wordWithBoundary = (if !featureWindow > 1 then 
										!wordDelimiter ^ newWord ^ !wordDelimiter 
									else 
										newWord ^ !wordDelimiter) in
			let wordWindow = (if (String.length wordWithBoundary) < !featureWindow then
									String.length wordWithBoundary
							else
								!featureWindow) in
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
													lastCharList
							in
							StringSet.iter
								(fun featureGram ->
									if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) featureGram then
										Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) featureGram) +. incrementAmount)
									else
										Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) featureGram incrementAmount;
									Array.set totalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) +. incrementAmount)
								)
								ngramFeatureSet
						)
						firstCharList
				)
				wordNgramList
	
	let use_score (word:string) = true
	
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
		if (not !mbdp) && (!phonemeWindow > 0) then
			PhonemeNgramCue.initialize !initialNgramCount;
		if (!featureFile <> "") && (!featureWindow > 0) then
			FeatureNgramCue.initialize !initialNgramCount
	with e ->
		close_in_noerr ic;
		raise e
else
	begin
		sentenceList := Std.input_list stdin;
		close_in stdin
	end;;

(* Setup initial value for utteranceDelimiter in the lexicon *)
Hashtbl.add lexicon !utteranceDelimiter 0.0;;

(* Updates lexicon with newly segmented words, and prints out segmented utterance *)
let rec lexicon_updater segmentation sentence updateFunctions (incrementAmount:float) =
	if (List.length segmentation) > 1 then
		begin
			let startChar = List.nth segmentation 0 in
			let endChar = List.nth segmentation 1 in
			let newWord = String.sub sentence startChar (endChar - startChar) in
			List.iter (fun updateFunc -> (updateFunc newWord incrementAmount)) updateFunctions; (* Calls all of the update functions in the list *)
			if (incrementAmount = 1.0) then
				begin
					printf "%s" newWord;
					if (List.length segmentation > 2) then
						printf "%s" !wordDelimiter
				end;
			lexicon_updater (List.tl segmentation) sentence updateFunctions incrementAmount
		end
	else
		();;


(* Backs-off from familiar word score to phoneme n-gram score. *)
let default_evidence_combiner word =
	let familiarScore = (FamiliarWordCue.eval_word word (+.)) in
	let phonemeScore = if (!phonemeWindow > 0) then (PhonemeNgramCue.eval_word word (+.)) else -.(log !badScore) in
	let syllableScore = if (!syllableWindow > 0) then (SyllableNgramCue.eval_word word (+.)) else infinity in
	if (!verbose) then
		printf "Familiar score for %s = %.15F\nSyllable score for %s = %.15F\nPhoneme score for %s = %.15F\n\n" word familiarScore word syllableScore word phonemeScore;
	if (FamiliarWordCue.use_score word) then
		familiarScore
	else
		if (!requireSyllabic && (SyllableNgramCue.use_score word)) || (syllableScore < infinity) then (* Can't syllabify word or score is acceptably low. *)
			syllableScore
		else
			phonemeScore;;

let additive_evidence_combiner word =
	let familiarScore = (FamiliarWordCue.eval_word word (+.)) in
	let phonemeScore = (PhonemeNgramCue.eval_word word (+.)) in
	if (!verbose) then
		printf "Familiar score for %s = %.15F\nPhoneme score for %s = %.15F\n\n" word familiarScore word phonemeScore;
	familiarScore +. phonemeScore;;

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
	(* Array.iter (fun (x, y) -> printf "(%F, %d)" x y) bestList; *)
	Array.map
	 	snd
		bestList;;

let eval_utterance = mbdp_outer;;

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

(* Use standard Viterbi-style search to just get top segmentation*)
let fast_search sentence =
	let bestStartList = eval_utterance sentence in
	[|(1.0, ((List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence]))|];;

let get_scored_segmentation_list = fast_search;;

(* Returns a tuple of a segmentation list and an unsegmented utterance of the type lexicon_updater needs from a pre-segmented utterance *)
let segmentation_of_segmented_sentence segmentedSentence =
	let sentence = replace ~rex:removeDelimitersPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
	let segmentedChars = (String.explode segmentedSentence) in
	let boundaryChar = List.nth (String.explode !wordDelimiter) 0 in
	let boundaryIndexes = List.mapi (fun index character -> 
										if (character = boundaryChar) then 
											index 
										else 
											0) 
									segmentedChars in
	(([0] @ (List.mapi (fun index boundary -> boundary - index) ((List.remove_all boundaryIndexes 0) @ [(String.length segmentedSentence)]))), sentence);;

(* Returns a segmentation list of the type lexicon_updater needs from a list of words *)
let segmentation_of_word_list words =
	segmentation_of_segmented_sentence (String.slice ~first:1 (List.fold_left (fun currentResult currentWord -> currentResult ^ !wordDelimiter ^ currentWord) "" words));;

(* Loop through utterances *)
let incremental_processor utteranceList = 
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit = 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let sentence = replace ~rex:removeDelimitersPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
					let segmentations = (if (!supervisedFor > utteranceCount) then 
											[|1.0, (fst (segmentation_of_segmented_sentence segmentedSentence))|]
										else 
											get_scored_segmentation_list sentence) in
					if (!displayLineNumbers) then
						printf "%d: " (utteranceCount + 1);
					Array.iter (fun (incrementAmount, segmentation) -> 
									lexicon_updater segmentation sentence [PhonemeNgramCue.update_evidence; SyllableNgramCue.update_evidence; FamiliarWordCue.update_evidence] incrementAmount
								) 
								segmentations;
					if (!printUtteranceDelimiter) then
						printf "%s" !utteranceDelimiter;		
					printf "\n";
					flush stdout;
					Hashtbl.replace lexicon !utteranceDelimiter ((Hashtbl.find lexicon !utteranceDelimiter) +. 1.0)
				end
		)
		utteranceList;;


(* Learn phonotactic model from gold corpus, and then resegment without updating phonotactic model *)
let two_pass_processor utteranceList = 
	printf "\nPhonotactic Pass:\n";
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit = 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let (segmentation, sentence) = segmentation_of_segmented_sentence segmentedSentence in
					lexicon_updater segmentation sentence [PhonemeNgramCue.update_evidence; SyllableNgramCue.update_evidence; FamiliarWordCue.update_evidence] 1.0; 
					printf "\n";
					flush stdout;
					Hashtbl.replace lexicon !utteranceDelimiter ((Hashtbl.find lexicon !utteranceDelimiter) +. 1.0)
				end
		)
		utteranceList;
	Hashtbl.clear lexicon;
	Hashtbl.add lexicon !utteranceDelimiter 0.0;
	totalWords := 0.0;
	printf "\nSegmentation Pass:\n";
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit = 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let sentence = replace ~rex:removeDelimitersPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
					let bestStartList = eval_utterance sentence in
					let segmentation = (List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence] in
					if (!displayLineNumbers) then
						printf "%d: " (utteranceCount + 1);
					lexicon_updater segmentation sentence [FamiliarWordCue.update_evidence] 1.0; 
					if (!printUtteranceDelimiter) then
						printf "%s" !utteranceDelimiter;		
					printf "\n";
					flush stdout;
					Hashtbl.replace lexicon !utteranceDelimiter ((Hashtbl.find lexicon !utteranceDelimiter) +. 1.0)
				end
		)
		utteranceList;;


(* let sentence_processor = two_pass_processor;; *)
let sentence_processor = incremental_processor;;

(*** ACTUAL START OF PROGRAM ***)
if (!interactive) then
	begin
		printf "Utterance number to process to: ";
		utteranceLimit := read_int ()
	end;;
		
sentence_processor !sentenceList;;

if (!interactive) then
	begin
		verbose := true;
		let eof = ref false in
		while (not !eof) do
			printf "\n#: ";
			try
				let command = String.nsplit (read_line ()) !wordDelimiter in
				match command with
				  "syllabify" :: args -> List.iter (fun arg -> printf "%s%s" (syllabify arg) !wordDelimiter) args; printf "\n"
				| "score" :: args -> List.map eval_word args; ()
				| "add" :: incrementAmount :: args -> let (segmentation, sentence) = segmentation_of_word_list args in lexicon_updater segmentation sentence [PhonemeNgramCue.update_evidence; SyllableNgramCue.update_evidence; FamiliarWordCue.update_evidence] (float_of_string incrementAmount); ()
 				| "help" :: [] -> printf "Available commands: \n    add INCREMENT-AMOUNT WORDS\tincreases the frequencies of WORDS by INCREMENT-AMOUNT\n    score WORDS\t\t\treturns the scores for WORDS\n    syllabify WORDS\t\tbreaks WORDS up into syllables\n"
				| _ -> printf "Unknown command.\n"
			with e ->
				eof := true;
			printf "\n"
		done
	end;;


(* Dump lexicon if requested *)
if !lexiconOut <> "" then
	let oc = open_out !lexiconOut in
	hash_fprint_float oc lexicon;
	close_out oc;;

(* Dump n-gram counts if requested *)
if !phonemeCountsOut <> "" then
	PhonemeNgramCue.dump !phonemeCountsOut;;

(* Dump n-gram counts if requested *)
if !phonemeCountsOut <> "" then
	FeatureNgramCue.dump !featureCountsOut;;

(* Dump n-gram counts if requested *)
if !syllableCountsOut <> "" then
	SyllableNgramCue.dump !syllableCountsOut;;
	