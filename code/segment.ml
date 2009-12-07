(*	
	PHOCUS: PHOnotactic CUe Segmenter
	Copyright (C) 2007-2009 Dan Blanchard.
	
	This file is part of PHOCUS.
	
	PHOCUS is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
    
	PHOCUS is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
    
	You should have received a copy of the GNU General Public License
	along with PHOCUS.  If not, see <http://www.gnu.org/licenses/>.
*)

open Pcre
open Printf
open ExtList
open ExtString
open ExtArray
open Num

module StringSet = Set.Make(String)

(* Converts floats to Nums *)
let num_of_float floatNum = 
	let (integer, decimal) = String.split (Printf.sprintf "%.14f" floatNum) "." in
	(num_of_string integer) +/ (num_of_string (decimal ^ " / " ^ (Printf.sprintf "%.0f" (10.0 ** (float_of_int ((String.length decimal)))))))

(* Converts floats to Nums *)
let num_of_float_string floatNum = 
	let (integer, decimal) = String.split floatNum "." in
	(num_of_string integer) +/ (num_of_string (decimal ^ " / " ^ (Printf.sprintf "%.0f" (10.0 ** (float_of_int ((String.length decimal)))))))

let e = num_of_float 2.71828183
let sixOverPiSquared = num_of_float 0.607927102
let printUtteranceDelimiter = ref false
let displayLineNumbers = ref false
let featureFile = ref ""
let badScore =  ref "0.0"
let initialNgramCount = ref "1.0"
let syllableDelimiter = ref "."
let wordDelimiter = ref " "
let utteranceDelimiter = ref "$"
let corpus = ref ""
let sentenceList = ref []
let lexiconOut = ref ""
let phonemeCountsOut = ref ""
let piecewiseCountsOut = ref ""
let featureCountsOut = ref ""
let syllableCountsOut = ref ""
let phonemeWindow = ref 1
let syllableWindow = ref 0
let featureWindow = ref 0
let jointProb = ref false
let smooth = ref false
let tokenPhonotactics = ref false
let interactive = ref false
let totalWords = ref (num_of_int 0)
let mbdp = ref false
let verbose = ref false
let countProposedNgrams = ref false
let ignoreWordBoundary = ref false
let utteranceLimit = ref 0
let lexicon = Hashtbl.create 10000
(* let decayFactor = ref "1.0" *)
let supervisedFor = ref 0
let requireSyllabic = ref false
let waitForStablePhonemeDist = ref false
let noLexicon = ref false
let waitUntilUtterance = ref 0
let stabilityThreshold = ref "0.99"
let goldPhonotactics = ref false
let typeDenominator = ref false
let uniformPhonotactics = ref false
let weightedSum = ref false
let subseqDenom = ref false
let currentOutputChannel = ref stdout
let semisupervisedUpdating = ref false
let initializeSyllables = ref false
let scorePiecewise = ref false

(* Process command-line arguments - this code must precede module definitions in order for their variables to get initialized correctly *)
let process_anon_args corpusFile = corpus := corpusFile
let arg_spec_list =["--badScore", Arg.Set_string badScore, " Score assigned when word length is less than window size (Default = 0.0)";
					"-bs", Arg.Set_string badScore, " Short for --badScore";
					(* "--decayFactor", Arg.Set_string decayFactor, " Exponent used to calculate memory decay.  (Default = 0.0, no decay)";
					"-df", Arg.Set_string decayFactor, " Short for --decayFactor"; *)
					"--featureChart", Arg.Set_string featureFile, " Feature chart file";
					"-fc", Arg.Set_string featureFile, " Short for --featureChart";					
					"--featureNgramsOut", Arg.Set_string featureCountsOut, " File to dump final feature n-gram counts to";
					"-fn", Arg.Set_string featureCountsOut, " Short for --featureNgramsOut";
					"--featureWindow", Arg.Set_int featureWindow, " Window size for feature n-grams";
					"-fw", Arg.Set_int featureWindow, " Short for --featureWindow";
					"--goldPhonotactics", Arg.Set goldPhonotactics, " Calculate phoneme n-gram scores based on their true frequencies in the gold corpus. ";
					"-gp", Arg.Set goldPhonotactics, " Short for --goldPhonotactics";
					"--hypotheticalPhonotactics", Arg.Set countProposedNgrams, " When evaluating hypothetical words' well-formedness, increment counts of all n-grams within proposed word. (Default = false)";
					"-hp", Arg.Set countProposedNgrams, " Short for --hypotheticalPhonotactics";
					"--ignoreWordBoundary", Arg.Set ignoreWordBoundary, " When calculating phoneme/syllable/etc. n-gram scores, do not include word boundary.";
					"-iw", Arg.Set ignoreWordBoundary, " Short for --ignoreWordBoundary";
					"--initialCount", Arg.Set_string initialNgramCount, " Count assigned to phonotactic largest n-grams before they are seen (default = 1.0)";
					"-ic", Arg.Set_string initialNgramCount, " Short for --initialCount";
					"--initializeSyllables", Arg.Set initializeSyllables, " Initialize syllable n-gram by finding all syllables in gold corpus and setting their counts to one in advance.";
					"-is", Arg.Set initializeSyllables, " Short for --initializeSyllables";
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
					"--noLexicon", Arg.Set noLexicon, " Only score words based on the phonotactics, and don't do 'familiar word' spotting.  Does NOT entail --tokenPhonotactics.";
					"-nl", Arg.Set noLexicon, " Short for --noLexicon";
					"--phonemeNgramsOut", Arg.Set_string phonemeCountsOut, " File to dump final phoneme n-gram counts to";
					"-pn", Arg.Set_string phonemeCountsOut, " Short for --phonemeNgramsOut";
					"--piecewiseCountsOut", Arg.Set_string piecewiseCountsOut, " File to dump final strictly two-piecewise counts to";
					"-pc", Arg.Set_string piecewiseCountsOut, " Short for --piecewiseCountsOut";
					"--phonemeWindow", Arg.Set_int phonemeWindow, " Window size for phoneme n-grams";
					"-pw", Arg.Set_int phonemeWindow, " Short for --phonemeWindow";
					"--printUtteranceDelimiter", Arg.Set printUtteranceDelimiter, " Print utterance delimiter at the end of each utterance";
					"-pu", Arg.Set printUtteranceDelimiter, " Short for --printUtteranceDelimiter";
					"--requireSyllabic", Arg.Set requireSyllabic, " Require each proposed word to contain at least one syllabic sound.  (Requires --featureChart that includes 'syllabic' as feature)";
					"-rs", Arg.Set requireSyllabic, " Short for --requireSyllabic";
					"--scorePiecewise", Arg.Set scorePiecewise, " Score potential words based on their Strictly 2-Piecewise factors (i.e., long distance pairs for vowel and consonantal harmony).";
					"-sp", Arg.Set scorePiecewise, " Short for --scorePiecewise";					
					"--semisupervisedUpdating", Arg.Set semisupervisedUpdating, " When doing semisupervised segmenting with the supervisedFor flag, resume learning process after supervised portion of corpus.";
					"-su", Arg.Set semisupervisedUpdating, " Short for --semisupervisedUpdating";
					"--stabilityThreshold", Arg.Set_string stabilityThreshold, " When --waitForStablePhonemeDist is enabled, all the ratio between all phoneme counts when they are updated must be greater than stabilityThreshold before model will start segmenting. (default = 0.99)";
					"-st", Arg.Set_string stabilityThreshold, " Short for --stabilityThreshold";
					"--subseqDenominator", Arg.Set subseqDenom, " For lexical score, calculate probability word is a word, rather than probability of word occuring in corpus.";
					"-sd", Arg.Set subseqDenom, " Short for --subseqDenom";
					"--supervisedFor", Arg.Set_int supervisedFor, " Number of utterances to use given word-boundaries for.  (Default = 0, unsupervised learner)";
					"-sf", Arg.Set_int supervisedFor, " Short for --supervisedFor";
					"--syllableNgramsOut", Arg.Set_string syllableCountsOut, " File to dump final syllable n-gram counts to";
					"-sn", Arg.Set_string syllableCountsOut, " Short for --syllableNgramsOut";
					"--syllableWindow", Arg.Set_int syllableWindow, " Window size for syllable n-grams";
					"-sw", Arg.Set_int syllableWindow, " Short for --syllableWindow";
					"--tokenPhonotactics", Arg.Set tokenPhonotactics, " Update phoneme n-gram counts once per word occurrence, instead of per word type.";
					"-tp", Arg.Set tokenPhonotactics, " Short for --tokenPhonotactics";
					"--typeDenominator", Arg.Set typeDenominator, " When evaluating familiar words, divide word count by total number of word tokens + total number of word types.  Should only be on for Venkataraman.";
					"-td", Arg.Set typeDenominator, " Short for --typeDenominator";					
					"--uniformPhonotactics", Arg.Set uniformPhonotactics, " Never update phonotactic n-gram counts.  Just use initial uniform distribution throughout.";
					"-up", Arg.Set uniformPhonotactics, " Short for --uniformPhonotactics";
					"--utteranceDelimiter", Arg.Set_string utteranceDelimiter, " Utterance delimiter"; 
					"-ud", Arg.Set_string utteranceDelimiter, " Short for --utteranceDelimiter"; 
					"--utteranceLimit", Arg.Set_int utteranceLimit, " Number of utterances in input corpus to process. (default = 0, which examines all)";
					"-ul", Arg.Set_int utteranceLimit, " Short for --utteranceLimit";
					"--verbose", Arg.Set verbose, " Print out scores for each possible segmentation of each utterance.";
					"-v", Arg.Set verbose, " Short for --verbose";
					"--waitForStablePhonemeDist", Arg.Set waitForStablePhonemeDist, " Do not start attempting to segment until phoneme unigram has stabilized.";
					"-wf", Arg.Set waitForStablePhonemeDist, " Short for --waitForStablePhonemeDist";
					"--waitUntilUtterance", Arg.Set_int waitUntilUtterance, " Do not start attempting to segment until we have reached the specified utterance number.";
					"-wu", Arg.Set_int waitUntilUtterance, " Short for --waitUntilUtterance";
					"--weightedSum", Arg.Set weightedSum, " Instead of using back-off model for score combination, use weighted sum with all cues weighted equally.";
					"-ws", Arg.Set weightedSum, " Short for --weightedSum";
					"--wordDelimiter", Arg.Set_string wordDelimiter, " Word delimiter";
					"-wd", Arg.Set_string wordDelimiter, " Short for --wordDelimiter"]

let usage = Sys.executable_name ^ " [-options] CORPUS";;
Arg.parse (Arg.align arg_spec_list) process_anon_args usage;;

if (!mbdp) then	initialNgramCount := "0.0";;

(* Convert command-line arguments to their Num versions *)
let badScoreNum = num_of_float_string !badScore
let stabilityThresholdNum = num_of_float_string !stabilityThreshold
let initialNgramCountNum = num_of_float_string !initialNgramCount;;
(* let decayNum = num_of_float_string !decayFactor;; *)


(* Read feature file, if specifed *)
if !featureFile <> "" then
	Featurechart.read_feature_file !featureFile;;

let hash_fprint_float file = Hashtbl.iter (fun key data -> fprintf file "%s\t%g\n" key data)
let hash_fprint_num file = Hashtbl.iter (fun key data -> fprintf file "%s\t%s\n" key (approx_num_exp 10 data))
let hash_fprint_int file = Hashtbl.iter (fun key data -> fprintf file "%s\t%d\n" key data)

let stringset_of_list stringList = List.fold_right StringSet.add stringList StringSet.empty

let removeDelimitersPattern = regexp (!wordDelimiter ^ "+")

let (diphthongs, vowels) = if (!featureFile <> "" ) then 
								StringSet.partition (fun phone -> (String.length phone) > 1) (Featurechart.phones_for_features (StringSet.singleton "+syllabic"))
							else
								((stringset_of_list ["9I";"OI";"9U"]), (stringset_of_list ["&";"6";"9";"A";"E";"I";"O";"U";"a";"e";"i";"o";"u";"R";"~";"M";"L"])) 
								
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

(* Returns a list of all the precedence pairs in the given word. *)
let get_pairs word = 
	let indexArray = Array.init (String.length word) (fun a -> a) in
	let seenPhonemes = Hashtbl.create 40 in
	Array.fold_left 
		(fun currentPairs currentFirstIndex ->
			let currentFirst = (String.sub word currentFirstIndex 1) in
			if (Hashtbl.mem seenPhonemes currentFirst) then
				currentPairs
			else
				begin
					Hashtbl.add seenPhonemes currentFirst true;
					if ((currentFirst = !wordDelimiter) && (currentFirstIndex = 0)) then
						currentPairs @ [currentFirst ^ (String.sub word 1 1)]
					else
						currentPairs @ (String.fold_left
											(fun pairs currentSecond ->
												(currentFirst ^ (String.of_char currentSecond)) :: pairs
											)
											[]
											(String.slice word ~first:(currentFirstIndex + 1)))
				end
		)
		[]
		indexArray


(* Takes a list of strings and returns all permutations of them of length (n + 1) *)		
let rec permutations permList delimiter n = 
	if n = 0 then
		permList
	else
		List.fold_left
			(fun resultList item -> 
				let concatList = List.map (fun x -> x ^ delimiter ^ item) permList in
				resultList @ concatList
			) 
			[]
			(permutations permList delimiter (n - 1));;

(* Quickly calculate P(n,r) without using factorials. *)
let rec numPermutationsRecurser n r =
	if (n >/ r) then
		r */ numPermutationsRecurser n (r +/ (num_of_int 1))
	else
		n

let numPermutations n r =
	numPermutationsRecurser n (n -/ r +/ (num_of_int 1))

	
module type CUE =
sig
	(* Returns probability that proposed word is a word.
	 * Takes function that determines how to combine scores in the case of sub-scores as argument. *)
	val eval_word : string -> (num -> num -> num) -> num
	
	(* Initializes any statistics to their default values, if necessary *)
	val initialize : num -> unit
	
	(* Takes a word and updates any evidence the cue keeps track of based on another occurrence of this word. *)
	val update_evidence : string -> num -> unit
	
	(* Dump the table associated with this cue to a file. *)
	val dump : string -> unit
	
	(* Check if we should back-off or use this cue*)
	val use_score : string -> bool
end

module FamiliarWordCue = 
struct
	let lastSeen = Hashtbl.create 10000
	let subseqCounts = Hashtbl.create 100000
	let tempSubseqCounts = Hashtbl.create 1000
	
	(* Returns frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine =
		if (Hashtbl.mem lexicon word) then
			let wordCount = Hashtbl.find lexicon word in
			let rawScore = 
				(if (not !mbdp) then
					let wordTypes = num_of_int ((Hashtbl.length lexicon) - 1) in (* Subtract one for initial utterance delimiter addition *)
					(wordCount // (if !subseqDenom then
										Hashtbl.find subseqCounts word
									else
										(if (!typeDenominator) then
											(!totalWords +/ wordTypes)
										else
											!totalWords)))
				else
					(((succ_num wordCount) // (succ_num !totalWords)) */ (square_num ((wordCount) // (succ_num wordCount)))))
			in
			rawScore (* */ (power_num decayNum ((succ_num (Hashtbl.find lexicon !utteranceDelimiter)) -/ (Hashtbl.find lastSeen word))) *)
		else
			badScoreNum
	
	let initialize initialCount = ()
	
	let dump dumpFile = 
		let oc = open_out dumpFile in
		hash_fprint_num oc lexicon;
		if (!subseqDenom) then
			begin
				fprintf oc "\n\nSubsequence Counts:\n";
				hash_fprint_num oc subseqCounts
			end;
		close_out oc
	
	let use_score (word:string) = (not !noLexicon) && (Hashtbl.mem lexicon word) 
	
	let update_subseq_count (newSeq:string) (incrementAmount:num)= 
		if Hashtbl.mem tempSubseqCounts newSeq then
			Hashtbl.replace tempSubseqCounts newSeq ((Hashtbl.find tempSubseqCounts newSeq) +/ incrementAmount)
		else
			Hashtbl.add tempSubseqCounts newSeq incrementAmount(* ;
					if (!verbose) then
						eprintf "\tUpdated count for %s to %s\n" newSeq (approx_num_exp 10 (Hashtbl.find tempSubseqCounts newSeq)) *)

	let commit_subseq_counts () = 
		Hashtbl.iter 
			(fun subseq count -> 
				if Hashtbl.mem subseqCounts subseq then
					Hashtbl.replace subseqCounts subseq ((Hashtbl.find subseqCounts subseq) +/ count)
				else
					Hashtbl.add subseqCounts subseq count(* ;
									if (!verbose) then
										eprintf "\tUpdated count for %s to %s\n" subseq (approx_num_exp 10 (Hashtbl.find subseqCounts subseq)) *)
			)
			tempSubseqCounts;
		Hashtbl.clear tempSubseqCounts
	
	let update_evidence (newWord:string) (incrementAmount:num)= 
		totalWords := !totalWords +/ incrementAmount;
		if Hashtbl.mem lexicon newWord then
			Hashtbl.replace lexicon newWord ((Hashtbl.find lexicon newWord) +/ incrementAmount)
		else
			Hashtbl.add lexicon newWord incrementAmount;
		Hashtbl.replace lastSeen newWord (Hashtbl.find lexicon !utteranceDelimiter);;		
		
	
end

module type NgramProbsSig = 
sig
	val prob_ngram : string -> string -> int -> (string, num) Hashtbl.t array -> num array -> num array -> (string, num) Hashtbl.t array -> num array -> num
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
	let prob_ngram_conditional prefix ngram n wordNgramCountsArray wordTotalNgramsArray ngramCountsArray initialCountsArray =
		if (n = 0) then
			begin
				if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
					(Hashtbl.find wordNgramCountsArray.(n) ngram) // wordTotalNgramsArray.(n)
				else if (initialCountsArray.(n) >/ (num_of_int 0)) then
					initialCountsArray.(n) // wordTotalNgramsArray.(n)
				else
					badScoreNum
			end
		else
			begin
				if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) // (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) // (Hashtbl.find ngramCountsArray.(n - 1) prefix)
						else if (initialCountsArray.(n - 1) >/ (num_of_int 0)) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) // initialCountsArray.(n - 1)
						else
							badScoreNum
					end
				else
					begin
						if (Hashtbl.mem ngramCountsArray.(n) ngram) then
							begin					
								if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
									(Hashtbl.find ngramCountsArray.(n) ngram) // (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
								else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
									(Hashtbl.find ngramCountsArray.(n) ngram) // (Hashtbl.find ngramCountsArray.(n - 1) prefix)
								else if (initialCountsArray.(n - 1) >/ (num_of_int 0)) then
									(Hashtbl.find ngramCountsArray.(n) ngram) // initialCountsArray.(n - 1)
								else
									badScoreNum
							end
						else if (initialCountsArray.(n) >/ (num_of_int 0)) then
							begin					
								if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
									initialCountsArray.(n) // (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
								else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
									initialCountsArray.(n) // (Hashtbl.find ngramCountsArray.(n - 1) prefix)
								else if (initialCountsArray.(n - 1) >/ (num_of_int 0)) then
									initialCountsArray.(n) // initialCountsArray.(n - 1)
								else
									badScoreNum
							end
						else
							badScoreNum
					end
			end;;

	(* The implementation of this function is NOT done yet. *)
	let prob_ngram_kneser_ney prefix ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray ngramCountsArray = 
		(* let d = discount n in *)
		if (n = 0) then
			(Hashtbl.find wordNgramCountsArray.(n) ngram) // wordTotalNgramsArray.(n)
		else
			begin
				if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) // (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							(Hashtbl.find wordNgramCountsArray.(n) ngram) // (Hashtbl.find ngramCountsArray.(n - 1) prefix)
						else
							badScoreNum
					end
				else 
					begin					
						if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
							(Hashtbl.find ngramCountsArray.(n) ngram) // (Hashtbl.find wordNgramCountsArray.(n - 1) prefix)
						else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
							(Hashtbl.find ngramCountsArray.(n) ngram) // (Hashtbl.find ngramCountsArray.(n - 1) prefix)
						else
							badScoreNum
					end								
			end;;

	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram_joint prefix ngram n wordNgramCountsArray wordTotalNgramsArray =
		(Hashtbl.find wordNgramCountsArray.(n) ngram) // wordTotalNgramsArray.(n);;

	(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
	let prob_ngram prefix ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray ngramCountsArray initialCountsArray = 
		match (!jointProb, !smooth) with 
			(false, true)  -> prob_ngram_kneser_ney prefix ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray ngramCountsArray
		|	(false, false) -> prob_ngram_conditional prefix ngram n wordNgramCountsArray wordTotalNgramsArray ngramCountsArray initialCountsArray
		| 	(_,_) -> prob_ngram_joint prefix ngram n wordNgramCountsArray wordTotalNgramsArray;;
end

module SyllableNgramCue : CUE =
struct
	open NgramProbs
	let ngramCountsArray = Array.init (!syllableWindow) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))))
	let totalNgramsArray = Array.init (!syllableWindow) (fun a -> (num_of_int 0))
	let typesWithCountArray = Array.init 3 (fun a -> num_of_int 0)
	let ngramList = List.init !syllableWindow (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	let maxOnsetLength = 3
	let maxCodaLength = 3
	let initialCountsArray = Array.init (!syllableWindow) (fun a -> num_of_int 0)
	
	(* Check if we can even syllabify word *)	
	let use_score (word:string) = ((syllabify word) = "") 
	
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount = 
		if (!initializeSyllables) then
			(* Don't forget to add one to number of syllable types for word boundary *)
			let numSyllables = ((num_of_string (input_line (Unix.open_process_in ("tr ' ' '\\n' < " ^ !corpus ^ 
				" | sort | uniq | ./syllabify.pl | tr '.' '\\n' | sort | uniq | wc -l | gsed 's/^[ \\t]*//'")))) +/ (num_of_int 1)) in			
			List.iter 
				(fun currentWindowSizeMinusOne ->
					initialCountsArray.(currentWindowSizeMinusOne) <- initialCount */ (power_num numSyllables (num_of_int (!syllableWindow - (currentWindowSizeMinusOne + 1)))) */ (num_of_int (!syllableWindow - currentWindowSizeMinusOne));
					totalNgramsArray.(currentWindowSizeMinusOne) <- (numPermutations numSyllables (num_of_int (currentWindowSizeMinusOne + 1))) */ initialCountsArray.(currentWindowSizeMinusOne);
					(* eprintf "Initial n-gram counts for length %d: %s\n\tTotal n-grams: %s\n\tNum syllables: %s\n" (currentWindowSizeMinusOne + 1) (approx_num_exp 10 initialCountsArray.(currentWindowSizeMinusOne)) (approx_num_exp 10 totalNgramsArray.(currentWindowSizeMinusOne)) (approx_num_exp 10 numSyllables);
					flush stderr *)
				)
				ngramList
		else
			()
	
	(* Collapse syllable array into string. *)
	let string_of_syllable_array syllableArray =
		Array.fold_left
			(fun a b  -> if (a <> "") then (a ^ !syllableDelimiter ^ b) else (a ^ b))
			""
			syllableArray
			
	(* Returns frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		let syllabifiedWord = syllabify word in
		let syllables = String.nsplit syllabifiedWord !syllableDelimiter in 
		if (syllabifiedWord <> "") then
			begin
				let wordNgramCountsArray = Array.init (!syllableWindow) (fun a -> Hashtbl.create 100) in
				let wordTotalNgramsArray = Array.init (!syllableWindow) (fun a -> num_of_int 0) in
				let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in
				let syllablesWithBoundary = Array.of_list (if not !ignoreWordBoundary then 
																 (if !syllableWindow > 1 then 
																	[!wordDelimiter] @ syllables @ [!wordDelimiter]
																else 
																	syllables @ [!wordDelimiter])
															else
																syllables) in										
				let score = ref (num_of_int 0) in
				if (Array.length syllablesWithBoundary) < !syllableWindow then
					badScoreNum
				else	
					begin
						List.iter (* Get n-gram counts of all size *)
							(fun currentWindowSizeMinusOne ->
								let currentSyllablesWithBoundary = Array.of_list (if not !ignoreWordBoundary then 
																				 (if currentWindowSizeMinusOne > 0 then 
																					[!wordDelimiter] @ syllables @ [!wordDelimiter]
																				else 
																					syllables @ [!wordDelimiter])
																			else
																				syllables) in										
								let ngramFirstSyllListLength = (Array.length currentSyllablesWithBoundary) - currentWindowSizeMinusOne in 
								let ngramFirstSyllList = List.init ngramFirstSyllListLength (fun a -> a) in
								List.iter (* Loop through all n-grams of current size *)
									(fun firstSyll ->
										let ngram = string_of_syllable_array (Array.sub currentSyllablesWithBoundary firstSyll (currentWindowSizeMinusOne + 1)) in
										if Hashtbl.mem wordNgramCountsArray.(currentWindowSizeMinusOne) ngram then
											Hashtbl.replace wordNgramCountsArray.(currentWindowSizeMinusOne) ngram (succ_num (Hashtbl.find wordNgramCountsArray.(currentWindowSizeMinusOne) ngram))
										else if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram (succ_num (Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram))
										else
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram (succ_num initialCountsArray.(currentWindowSizeMinusOne));
									)
									ngramFirstSyllList;
								wordTotalNgramsArray.(currentWindowSizeMinusOne) <- (totalNgramsArray.(currentWindowSizeMinusOne) +/ (num_of_int ngramFirstSyllListLength))
							)
							ngramList;
						(*  countProposedNgrams does not seem to work properly with syllable BIGRAMS without subseqDenom *)
						let currentTotalNgramsArray = (if !countProposedNgrams then wordTotalNgramsArray else totalNgramsArray) in
						let currentNgramCountsArray = (if !countProposedNgrams then wordNgramCountsArray else ngramCountsArray) in
						if (currentTotalNgramsArray.(0) >/ (num_of_int 0)) && (Hashtbl.mem currentNgramCountsArray.(0) !wordDelimiter) then
							begin
								score := (num_of_int 1);
								score := !score */ (if !syllableWindow > 1 then 
															num_of_int 1
														else
															(num_of_int 1) // ((num_of_int 1) -/ ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) // currentTotalNgramsArray.(0))));
								List.iter (* Get ngram scores *)
									(fun firstSyll ->
										let ngramSyllableArray = Array.sub syllablesWithBoundary firstSyll !syllableWindow in
										let prefix = string_of_syllable_array (Array.sub ngramSyllableArray 0 (!syllableWindow - 1)) in
										let ngram = string_of_syllable_array ngramSyllableArray in
										let ngramScore = prob_ngram prefix ngram (!syllableWindow - 1) currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray ngramCountsArray initialCountsArray in
										(* eprintf "\tPrefix = '%s'\n\tNgram score for '%s' = %s\n" prefix ngram (approx_num_exp 10 ngramScore);
										flush stderr; *)
										score := (combine !score ngramScore)
									)
									(List.init ((Array.length syllablesWithBoundary) - (!syllableWindow - 1)) (fun a -> a));
								!score
							end
						else
							num_of_int 0
					end
			end
		else
			num_of_int 0
		
	let update_evidence (newWord:string) (incrementAmount:num)= 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			let syllabifiedWord = syllabify newWord in
			(* eprintf "\nSyllabified word: %s\n" syllabifiedWord; *)
			let syllables = String.nsplit syllabifiedWord !syllableDelimiter in 
			let syllablesWithBoundary = Array.of_list (if not !ignoreWordBoundary then 
															 (if !syllableWindow > 1 then 
																[!wordDelimiter] @ syllables @ [!wordDelimiter]
															else 
																syllables @ [!wordDelimiter])
														else
															syllables) in										
			let wordWindow = (if (Array.length syllablesWithBoundary) < !syllableWindow then
									Array.length syllablesWithBoundary
							else
								!syllableWindow) in
			let wordNgramList = List.init wordWindow (fun a -> a) in
			List.iter (* Get n-gram counts of all size *)
				(fun currentWindowSizeMinusOne ->
					let currentSyllablesWithBoundary = Array.of_list (if not !ignoreWordBoundary then 
																		(if currentWindowSizeMinusOne > 0 then 
																			[!wordDelimiter] @ syllables @ [!wordDelimiter]
																		else 
																			syllables @ [!wordDelimiter])
																	else
																		syllables) in										
					let ngramFirstSyllListLength = (Array.length currentSyllablesWithBoundary) - currentWindowSizeMinusOne in 
					let ngramFirstSyllList = List.init ngramFirstSyllListLength (fun a -> a) in
					List.iter (* Loop through all n-grams of current size *)
						(fun firstSyll ->
							let ngram = string_of_syllable_array (Array.sub currentSyllablesWithBoundary firstSyll (currentWindowSizeMinusOne + 1)) in
							if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then								
								Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) +/ incrementAmount)
							else
								Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram (initialCountsArray.(currentWindowSizeMinusOne) +/ incrementAmount)
							(* eprintf "\tSyllable n-gram: %s\n" ngram;
							flush stderr *)
						)
						ngramFirstSyllList;
					totalNgramsArray.(currentWindowSizeMinusOne) <- (totalNgramsArray.(currentWindowSizeMinusOne) +/ ((num_of_int ngramFirstSyllListLength) */ incrementAmount))
				)
				wordNgramList
	
	(* Dump the table associated with this cue to a file. *)
	let dump dumpFile = 
		let oc = open_out dumpFile in
		List.iter
			(fun currentWindowSizeMinusOne ->
				fprintf oc "CURRENT WINDOW SIZE: %d\n" (currentWindowSizeMinusOne + 1);
				hash_fprint_num oc ngramCountsArray.(currentWindowSizeMinusOne);
			)
			ngramList;
		close_out oc
end

module PhonemeNgramCue : CUE =
struct
	open NgramProbs
	
	let ngramCountsArray = Array.init (!phonemeWindow) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2))))) 
	let totalNgramsArray = Array.init (!phonemeWindow) (fun a -> num_of_int 0)
	let typesWithCountArray = Array.init 3 (fun a -> num_of_int 0)
	let ngramList = List.init !phonemeWindow (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	let oldUnigramCounts = Hashtbl.create 40
	let hasStabilized = ref false
	let initialCountsArray = Array.init (!phonemeWindow) (fun a -> num_of_int 0)
	
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount =
		let phonemeList = !wordDelimiter :: (Std.input_list (Unix.open_process_in ("gsed -r 's/(.)/\\1\\n/g' " ^ !corpus ^ " | gsed '/^$/d' | sort -u | gsed '/[ \\t]/d'"))) in
		let numPhonemes = num_of_int (List.length phonemeList) in
		List.iter 
			(fun currentWindowSizeMinusOne ->
				let phonemePermutationList = permutations phonemeList "" currentWindowSizeMinusOne in
				let currentIncrementAmount = initialCount */ (power_num numPhonemes (num_of_int (!phonemeWindow - (currentWindowSizeMinusOne + 1)))) */ (num_of_int (!phonemeWindow - currentWindowSizeMinusOne)) in
				initialCountsArray.(currentWindowSizeMinusOne) <- initialCount */ (power_num numPhonemes (num_of_int (!phonemeWindow - (currentWindowSizeMinusOne + 1)))) */ (num_of_int (!phonemeWindow - currentWindowSizeMinusOne));
				List.iter
					(fun ngram ->
						Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram currentIncrementAmount;
						totalNgramsArray.(currentWindowSizeMinusOne) <- totalNgramsArray.(currentWindowSizeMinusOne) +/ currentIncrementAmount
					)
					phonemePermutationList;
				if (currentWindowSizeMinusOne = 0) then
					List.iter
						(fun ngram ->
							Hashtbl.add oldUnigramCounts ngram currentIncrementAmount;
						)
						phonemePermutationList
				else
					()
			)
			ngramList

	(* Returns frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		let wordNgramCountsArray = Array.init (!phonemeWindow) (fun a -> Hashtbl.create 100) in
		let wordTotalNgramsArray = Array.init (!phonemeWindow) (fun a -> num_of_int 0) in
		let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in
		let wordWithBoundary = (if not !ignoreWordBoundary then 
									(if !phonemeWindow > 1 then 
										!wordDelimiter ^ word ^ !wordDelimiter 
									else 
										word ^ !wordDelimiter)
								else
									word) in							
		let wordTypes = num_of_int (Hashtbl.length lexicon) in (* Don't need to add one for MBDP because the initial addition of the utterance delimiter makes this one higher *)
		let totalWordsNum = (if !mbdp then (succ_num !totalWords) else !totalWords) in
		let score = ref (num_of_int 0) in
		if (String.length wordWithBoundary) < !phonemeWindow then
			badScoreNum
		else	
			begin
				List.iter (* Get n-gram counts of all size *)
					(fun currentWindowSizeMinusOne ->
						let currentWordWithBoundary = (if not !ignoreWordBoundary then 
													(if currentWindowSizeMinusOne > 0 then 
														!wordDelimiter ^ word ^ !wordDelimiter 
													else 
														word ^ !wordDelimiter)
												else
													word) in							
						let ngramFirstCharListLength = (String.length currentWordWithBoundary) - currentWindowSizeMinusOne in 
						let ngramFirstCharList = List.init ngramFirstCharListLength (fun a -> a) in
						List.iter (* Loop through all n-grams of current size *)
							(fun firstChar ->
								let ngram = String.sub currentWordWithBoundary firstChar (currentWindowSizeMinusOne + 1) in
								if Hashtbl.mem wordNgramCountsArray.(currentWindowSizeMinusOne) ngram then
									Hashtbl.replace wordNgramCountsArray.(currentWindowSizeMinusOne) ngram (succ_num (Hashtbl.find wordNgramCountsArray.(currentWindowSizeMinusOne) ngram))
								else if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
									Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram (succ_num (Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram))
								else
									Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram (succ_num initialCountsArray.(currentWindowSizeMinusOne));
							)
							ngramFirstCharList;
						wordTotalNgramsArray.(currentWindowSizeMinusOne) <- (totalNgramsArray.(currentWindowSizeMinusOne) +/ (num_of_int ngramFirstCharListLength))
					)
					ngramList;
				let currentTotalNgramsArray = (if !countProposedNgrams then wordTotalNgramsArray else totalNgramsArray) in
				let currentNgramCountsArray = (if !countProposedNgrams then wordNgramCountsArray else ngramCountsArray) in								
				if (not !mbdp) then
					score := wordTypes // (wordTypes +/ totalWordsNum)
				else
					score := num_of_int 1;
				score := !score */ (if !phonemeWindow > 1 then 
											num_of_int 1
										else
											(num_of_int 1) // ((num_of_int 1) -/ ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) // currentTotalNgramsArray.(0)))); (* This term is necessary because the empty word is not really in the lexicon. *)
				(* eprintf "basePhonemeScore = %F\twordDelimiterCount = %F\twordtotal = %F\n" !score (Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) currentTotalNgramsArray.(0);  *)
				List.iter (* Get ngram scores *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar !phonemeWindow in
						let ngramScore = prob_ngram (String.sub ngram 0 (!phonemeWindow - 1)) ngram (!phonemeWindow - 1) currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray ngramCountsArray initialCountsArray in
						(* eprintf "\tNgram score for %s = %F\n" ngram ngramScore; *)
						score := (combine !score ngramScore)
					)
					(List.init ((String.length wordWithBoundary) - (!phonemeWindow - 1)) (fun a -> a));
				if (not !mbdp) then
					!score
				else
					begin
						let adjustment = (sixOverPiSquared */ (wordTypes // (totalWordsNum))) */ (square_num ((pred_num wordTypes) // wordTypes)) in
						(* eprintf "Score adjustment = %F\n" adjustment; *)
						(* eprintf "Raw phoneme score = %F\n" !score; *)
						!score */ adjustment
					end
			end
	
	let update_evidence (newWord:string) (incrementAmount:num) = 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			let wordWithBoundary = (if not !ignoreWordBoundary then 
										(if !phonemeWindow > 1 then 
											!wordDelimiter ^ newWord ^ !wordDelimiter 
										else 
											newWord ^ !wordDelimiter)
									else
										newWord) in
			let wordWindow = (if (String.length wordWithBoundary) < !phonemeWindow then
									String.length wordWithBoundary
							else
								!phonemeWindow) in
			let wordNgramList = List.init wordWindow (fun a -> a) in
			List.iter (* Get n-gram counts of all size *)
				(fun currentWindowSizeMinusOne ->
					let currentWordWithBoundary = (if not !ignoreWordBoundary then 
												(if currentWindowSizeMinusOne > 0 then 
													!wordDelimiter ^ newWord ^ !wordDelimiter 
												else 
													newWord ^ !wordDelimiter)
											else
												newWord) in							
					let ngramFirstCharListLength = (String.length currentWordWithBoundary) - currentWindowSizeMinusOne in 
					let ngramFirstCharList = List.init ngramFirstCharListLength (fun a -> a) in
					List.iter (* Loop through all n-grams of current size *)
						(fun firstChar ->
							let ngram = String.sub currentWordWithBoundary firstChar (currentWindowSizeMinusOne + 1) in
							if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
								Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) +/ incrementAmount)
							else
								Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram (initialCountsArray.(currentWindowSizeMinusOne) +/ incrementAmount);
						)
						ngramFirstCharList;
					totalNgramsArray.(currentWindowSizeMinusOne) <- (totalNgramsArray.(currentWindowSizeMinusOne) +/ ((num_of_int ngramFirstCharListLength) */ incrementAmount))
				)
				wordNgramList
	
	
	let use_score (utterance:string) =
		hasStabilized := !hasStabilized || ((not (Hashtbl.mem lexicon utterance)) && Hashtbl.fold
																						(fun phoneme count wasStable ->
																							let oldCount = Hashtbl.find oldUnigramCounts phoneme in
																							Hashtbl.replace oldUnigramCounts phoneme count;
																							(* if (not !hasStabilized) then
																								eprintf "utterance: %s\tphoneme: %s\toldCount: %F\tnewCount: %F\tpercentage: %F\n" utterance phoneme oldCount count (oldCount /. count)
																							else
																								(); *)
																							(wasStable && (oldCount // count >/ stabilityThresholdNum))
																						)
																						ngramCountsArray.(0) 
																						true);
		!hasStabilized
		

	(* Dump the table associated with this cue to a file. *)
	let dump dumpFile = 
		let oc = open_out dumpFile in
		List.iter
			(fun currentWindowSizeMinusOne ->
				fprintf oc "CURRENT WINDOW SIZE: %d\n" (currentWindowSizeMinusOne + 1);
				hash_fprint_num oc ngramCountsArray.(currentWindowSizeMinusOne);
			)
			ngramList;
		close_out oc
end

(* Stricly-2-Piecewise Learner*)
module PhonemePiecewiseCue : CUE =
struct
	open NgramProbs
	
	let ngramCountsArray = Array.init 2 (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2))))) 
	let totalNgramsArray = Array.init 2 (fun a -> num_of_int 0)
	let hasStabilized = ref false
	let typesWithCountArray = Array.init 3 (fun a -> num_of_int 0)
	let initialCountsArray = Array.init 2 (fun a -> num_of_int 0)
		
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount =
		let phonemeList = !wordDelimiter :: (Std.input_list (Unix.open_process_in ("gsed -r 's/(.)/\\1\\n/g' " ^ !corpus ^ " | gsed '/^$/d' | sort -u | gsed '/[ \\t]/d'"))) in
		let numPhonemes = num_of_int (List.length phonemeList) in
		let pairPermutationList = permutations phonemeList "" 1 in
		let pairIncrementAmount = initialCount in
		let singleIncrementAmount = initialCount */ numPhonemes */ (num_of_int 2) in
		initialCountsArray.(1) <- pairIncrementAmount;
		initialCountsArray.(0) <- singleIncrementAmount;
		List.iter
			(fun ngram ->
				Hashtbl.add ngramCountsArray.(1) ngram pairIncrementAmount;
				totalNgramsArray.(1) <- totalNgramsArray.(1) +/ pairIncrementAmount
			)
			pairPermutationList;
		List.iter
			(fun ngram ->
				Hashtbl.add ngramCountsArray.(0) ngram singleIncrementAmount;
				totalNgramsArray.(0) <- totalNgramsArray.(0) +/ singleIncrementAmount
			)
			phonemeList

	(* Returns frequency that word occurs in lexicon. *)
	let eval_word (word:string) combine = 
		let wordNgramCountsArray = Array.init 2 (fun a -> Hashtbl.create 100) in
		let wordTotalNgramsArray = Array.init 2 (fun a -> num_of_int 0) in
		let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in
		let wordWithBoundary = (if not !ignoreWordBoundary then 											 
										!wordDelimiter ^ word ^ !wordDelimiter 
									else
										word) in
		let wordTypes = num_of_int (Hashtbl.length lexicon) in (* Don't need to add one for MBDP because the initial addition of the utterance delimiter makes this one higher *)
		let totalWordsNum = (if !mbdp then (succ_num !totalWords) else !totalWords) in
		let score = ref (num_of_int 0) in
		if (String.length wordWithBoundary) < !phonemeWindow then
			badScoreNum
		else	
			begin
				let wordPiecewisePairs = get_pairs wordWithBoundary in
				List.iter (* Get pair and single counts *)
					(fun ngram ->						
						if Hashtbl.mem wordNgramCountsArray.(1) ngram then
							Hashtbl.replace wordNgramCountsArray.(1) ngram (succ_num (Hashtbl.find wordNgramCountsArray.(1) ngram))
						else if Hashtbl.mem ngramCountsArray.(1) ngram then
							Hashtbl.add wordNgramCountsArray.(1) ngram (succ_num (Hashtbl.find ngramCountsArray.(1) ngram))
						else
							Hashtbl.add wordNgramCountsArray.(1) ngram (succ_num initialCountsArray.(1));

						let firstChar = String.sub ngram 0 1 in
						if Hashtbl.mem wordNgramCountsArray.(0) firstChar then
							Hashtbl.replace wordNgramCountsArray.(0) firstChar (succ_num (Hashtbl.find wordNgramCountsArray.(0) firstChar))
						else if Hashtbl.mem ngramCountsArray.(0) firstChar then
							Hashtbl.add wordNgramCountsArray.(0) firstChar (succ_num (Hashtbl.find ngramCountsArray.(0) firstChar))
						else
							Hashtbl.add wordNgramCountsArray.(0) firstChar (succ_num initialCountsArray.(0))
					)
					wordPiecewisePairs;
				wordTotalNgramsArray.(1) <- totalNgramsArray.(1) +/ (num_of_int (List.length wordPiecewisePairs));
				wordTotalNgramsArray.(0) <- totalNgramsArray.(0) +/ (num_of_int (List.length wordPiecewisePairs));
				let currentTotalNgramsArray = (if !countProposedNgrams then wordTotalNgramsArray else totalNgramsArray) in
				let currentNgramCountsArray = (if !countProposedNgrams then wordNgramCountsArray else ngramCountsArray) in								
				if (not !mbdp) then
					score := wordTypes // (wordTypes +/ totalWordsNum)
				else
					score := num_of_int 1;
				score := !score */ (if !phonemeWindow > 1 then 
											num_of_int 1
										else
											(num_of_int 1) // ((num_of_int 1) -/ ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) // currentTotalNgramsArray.(0)))); (* This term is necessary because the empty word is not really in the lexicon. *)
				(* eprintf "basePhonemeScore = %F\twordDelimiterCount = %F\twordtotal = %F\n" !score (Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) currentTotalNgramsArray.(0);  *)
				List.iter (* Get ngram scores *)
					(fun ngram ->
						let ngramScore = prob_ngram (String.sub ngram 0 1) ngram 1 currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray ngramCountsArray initialCountsArray in
						(* eprintf "\tNgram score for %s = %F\n" ngram ngramScore; *)
						score := (combine !score ngramScore)
					)
					wordPiecewisePairs;
				if (not !mbdp) then
					!score
				else
					begin
						let adjustment = (sixOverPiSquared */ (wordTypes // (totalWordsNum))) */ (square_num ((pred_num wordTypes) // wordTypes)) in
						(* eprintf "Score adjustment = %F\n" adjustment; *)
						(* eprintf "Raw phoneme score = %F\n" !score; *)
						!score */ adjustment
					end
			end
	
	let update_evidence (newWord:string) (incrementAmount:num) = 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			begin
				let wordWithBoundary = (if (not !ignoreWordBoundary) then 
											(!wordDelimiter ^ newWord ^ !wordDelimiter) 
										else
											newWord) in
				let wordPiecewisePairs = (get_pairs wordWithBoundary) in
				totalNgramsArray.(1) <- totalNgramsArray.(1) +/ ((num_of_int (List.length wordPiecewisePairs)) */ incrementAmount);
				totalNgramsArray.(0) <- totalNgramsArray.(0) +/ ((num_of_int (List.length wordPiecewisePairs)) */ incrementAmount);
				List.iter (* Get pair and single counts *)
					(fun ngram ->						
						let firstChar = (String.sub ngram 0 1) in
	 					if Hashtbl.mem ngramCountsArray.(1) ngram then
							Hashtbl.replace ngramCountsArray.(1) ngram ((Hashtbl.find ngramCountsArray.(1) ngram) +/ incrementAmount)
						else
							Hashtbl.add ngramCountsArray.(1) ngram (initialCountsArray.(1) +/ incrementAmount);
						if Hashtbl.mem ngramCountsArray.(0) firstChar then
							Hashtbl.replace ngramCountsArray.(0) firstChar ((Hashtbl.find ngramCountsArray.(0) firstChar) +/ incrementAmount)
						else
							Hashtbl.add ngramCountsArray.(0) firstChar (initialCountsArray.(0) +/ incrementAmount)
					)
					wordPiecewisePairs
			end
	
	
	let use_score (word:string) = true
		

	(* Dump the table associated with this cue to a file. *)
	let dump dumpFile = 
		let oc = open_out dumpFile in
		List.iter
			(fun currentWindowSizeMinusOne ->
				fprintf oc "CURRENT WINDOW SIZE: %d\n" (currentWindowSizeMinusOne + 1);
				hash_fprint_num oc ngramCountsArray.(currentWindowSizeMinusOne);
			)
			[0;1];
		close_out oc
end


module FeatureNgramCue : CUE =
struct
	open NgramProbs
	let ngramCountsArray = Array.init (!featureWindow) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))))
	let totalNgramsArray = Array.init (!phonemeWindow) (fun a -> num_of_int 0)
	let typesWithCountArray = Array.init 3 (fun a -> num_of_int 0)
	let cartesianProductCache = Hashtbl.create 10000
	let ngramList = List.init !featureWindow (fun a -> a) (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)
	let initialCountsArray = Array.init (!phonemeWindow) (fun a -> num_of_int 0)
	
	(* Initialize the counts so we get a uniform distribution *)
	let initialize initialCount =
		let featureList = Std.input_list (Unix.open_process_in ("head -n 1 " ^ !featureFile ^ " | gsed -r 's/^\\t//' | tr '\\t' '\\n'")) in
		let featureValueList = (List.map (fun a -> "+" ^ a) featureList) @ (List.map (fun a -> "-" ^ a) featureList) in
		let numFeatureValues =  num_of_int (List.length featureValueList) in
		List.iter 
			(fun currentWindowSizeMinusOne ->
				let featurePermutationList = permutations featureValueList "" currentWindowSizeMinusOne in
				let currentIncrementAmount = initialCount */ (power_num numFeatureValues (num_of_int (!featureWindow - (currentWindowSizeMinusOne + 1)))) */ (num_of_int (!featureWindow - currentWindowSizeMinusOne)) in
				initialCountsArray.(currentWindowSizeMinusOne) <- initialCount */ (power_num numFeatureValues (num_of_int (!featureWindow - (currentWindowSizeMinusOne + 1)))) */ (num_of_int (!featureWindow - currentWindowSizeMinusOne));
				List.iter
					(fun ngram -> 
						Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram currentIncrementAmount;
						totalNgramsArray.(currentWindowSizeMinusOne) <- totalNgramsArray.(currentWindowSizeMinusOne) +/ currentIncrementAmount
					)
					featurePermutationList
			)
			ngramList
		
	let eval_word (word:string) combine = 
		let wordNgramCountsArray = Array.init (!featureWindow) (fun a -> Hashtbl.create 100) in
		let wordTotalNgramsArray = Array.init (!phonemeWindow) (fun a -> num_of_int 0) in
		let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in
		let wordWithBoundary = (if not !ignoreWordBoundary then 
									(if !featureWindow > 1 then 
										!wordDelimiter ^ word ^ !wordDelimiter 
									else 
										word ^ !wordDelimiter)
								else
									word) in							
		let wordTypes = num_of_int (Hashtbl.length lexicon) in (* Don't need to add one for MBDP because the initial addition of the utterance delimiter makes this one higher *)
		let totalWordsNum = (if !mbdp then (succ_num !totalWords) else !totalWords) in
		let score = ref (num_of_int 0) in
			if (String.length wordWithBoundary) < !featureWindow then
			badScoreNum
		else	
			begin
				List.iter (* Get feature n-gram counts of all size *)
					(fun currentWindowSizeMinusOne ->						
						let currentWordWithBoundary = (if not !ignoreWordBoundary then 
													(if currentWindowSizeMinusOne > 0 then 
														!wordDelimiter ^ word ^ !wordDelimiter 
													else 
														word ^ !wordDelimiter)
												else
													word) in							
						let firstCharListForBundles = Array.init (String.length currentWordWithBoundary) (fun a -> a) in
						let firstCharList = List.init ((String.length currentWordWithBoundary) - currentWindowSizeMinusOne) (fun a -> a) in
						let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
												(fun firstChar ->
													let phoneme = String.sub currentWordWithBoundary firstChar 1 in
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
															let subWord = String.sub currentWordWithBoundary firstChar (lastChar + 1) in
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
											Hashtbl.replace wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram (succ_num (Hashtbl.find wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram))
										else if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) featureGram then
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram (succ_num (Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) featureGram))
										else
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram (succ_num initialCountsArray.(currentWindowSizeMinusOne));											
										wordTotalNgramsArray.(currentWindowSizeMinusOne) <- (succ_num totalNgramsArray.(currentWindowSizeMinusOne))
									)
									ngramFeatureSet
							)
							firstCharList
					)
					ngramList;
				let currentTotalNgramsArray = (if !countProposedNgrams then wordTotalNgramsArray else totalNgramsArray) in
				let currentNgramCountsArray = (if !countProposedNgrams then wordNgramCountsArray else ngramCountsArray) in								
				if (not !mbdp) then
					score := wordTypes // (wordTypes +/ totalWordsNum)
				else
					score := num_of_int 1;
				score := !score */ (if !phonemeWindow > 1 then 
											num_of_int 1
										else
											(num_of_int 1) // ((num_of_int 1) -/ ((Hashtbl.find currentNgramCountsArray.(0) !wordDelimiter) // currentTotalNgramsArray.(0))));
				List.iter (* Get ngram scores *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar !featureWindow in
						let ngramScore = prob_ngram (String.sub ngram 0 (!featureWindow - 1)) ngram (!featureWindow - 1) currentNgramCountsArray currentTotalNgramsArray wordTypesWithCountArray ngramCountsArray initialCountsArray in
						(* eprintf "\tNgram score for %s = %F\n" ngram ngramScore; *)
						score := (combine !score ngramScore)
					)
					(List.init ((String.length wordWithBoundary) - (!featureWindow - 1)) (fun a -> a));
				if (not !mbdp) then
					!score
				else
					begin
						let adjustment = (sixOverPiSquared */ (wordTypes // (totalWordsNum))) */ (square_num ((pred_num wordTypes) // wordTypes)) in
						(* eprintf "Score adjustment = %F\n" adjustment; *)
						(* eprintf "Raw phoneme score = %F\n" !score; *)
						!score */ adjustment
					end
			end
	
	let update_evidence (newWord:string) (incrementAmount:num)= 
		if (!tokenPhonotactics || (not (Hashtbl.mem lexicon newWord))) then
			let wordWithBoundary = (if not !ignoreWordBoundary then 
										(if !featureWindow > 1 then 
											!wordDelimiter ^ newWord ^ !wordDelimiter 
										else 
											newWord ^ !wordDelimiter)
									else
										newWord) in
			let wordWindow = (if (String.length wordWithBoundary) < !featureWindow then
									String.length wordWithBoundary
							else
								!featureWindow) in
			let wordNgramList = List.init wordWindow (fun a -> a) in
			List.iter (* Get feature n-gram counts of all size *)
				(fun currentWindowSizeMinusOne ->											
					let currentWordWithBoundary = (if not !ignoreWordBoundary then 
												(if currentWindowSizeMinusOne > 0 then 
													!wordDelimiter ^ newWord ^ !wordDelimiter 
												else 
													newWord ^ !wordDelimiter)
											else
												newWord) in							
					let firstCharListForBundles = Array.init (String.length currentWordWithBoundary) (fun a -> a) in
					let firstCharList = List.init ((String.length currentWordWithBoundary) - currentWindowSizeMinusOne) (fun a -> a) in
					let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
											(fun firstChar ->
												let phoneme = String.sub currentWordWithBoundary firstChar 1 in												
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
														let subWord = String.sub currentWordWithBoundary firstChar (lastChar + 1) in
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
										Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) featureGram) +/ incrementAmount)
									else
										Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) featureGram (initialCountsArray.(currentWindowSizeMinusOne) +/ incrementAmount);
									totalNgramsArray.(currentWindowSizeMinusOne) <- (totalNgramsArray.(currentWindowSizeMinusOne) +/ incrementAmount)
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
				hash_fprint_num oc ngramCountsArray.(currentWindowSizeMinusOne);
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
			PhonemeNgramCue.initialize initialNgramCountNum;
		if (!scorePiecewise) && (not !goldPhonotactics) then
			PhonemePiecewiseCue.initialize initialNgramCountNum;
		if (not !mbdp) && (!syllableWindow > 0) then
			SyllableNgramCue.initialize initialNgramCountNum;
		if (!featureFile <> "") && (!featureWindow > 0) then
			FeatureNgramCue.initialize initialNgramCountNum
	with e ->
		close_in_noerr ic;
		raise e
else
	begin
		sentenceList := Std.input_list stdin;
		close_in stdin
	end;;

(* Setup initial value for utteranceDelimiter in the lexicon *)
Hashtbl.add lexicon !utteranceDelimiter (num_of_int 0);;

(* Updates lexicon with newly segmented words, and prints out segmented utterance *)
let rec lexicon_updater segmentation sentence updateFunctions (incrementAmount:num) =
	if (List.length segmentation) > 1 then
		begin
			let startChar = List.nth segmentation 0 in
			let endChar = List.nth segmentation 1 in
			let newWord = String.sub sentence startChar (endChar - startChar) in
			List.iter (fun updateFunc -> (updateFunc newWord incrementAmount)) updateFunctions; (* Calls all of the update functions in the list *)
			if (incrementAmount =/ (num_of_int 1)) then
				begin
					fprintf !currentOutputChannel "%s" newWord;
					if (List.length segmentation > 2) then
						fprintf !currentOutputChannel "%s" !wordDelimiter
				end;
			lexicon_updater (List.tl segmentation) sentence updateFunctions incrementAmount
		end
	else
		FamiliarWordCue.commit_subseq_counts ();;

(* These two functions are used for updating the subsequence counts if we're doing a supervised pass through the corpus.*)
let rec subsequence_updater_inner subUtterance firstChar lastChar =
	if firstChar <= lastChar then
		begin
			let newSubUtterance = String.sub subUtterance firstChar ((lastChar + 1) - firstChar) in
			FamiliarWordCue.update_subseq_count newSubUtterance (num_of_int 1);
			subsequence_updater_inner subUtterance (firstChar + 1) lastChar 
		end
	else
		();;

let subsequence_updater_outer sentence =
	let lastCharList = Array.init (String.length sentence) (fun a -> a) in
	Array.iter
		(fun lastChar ->
			let subUtterance = String.sub sentence 0 (lastChar + 1) in
			FamiliarWordCue.update_subseq_count subUtterance (num_of_int 1);						
			subsequence_updater_inner subUtterance 1 lastChar
		)
		lastCharList;;

(* Prints out segmented utterance *)
let rec print_segmented_utterance segmentation sentence (incrementAmount:num) =
	if (List.length segmentation) > 1 then
		begin
			let startChar = List.nth segmentation 0 in
			let endChar = List.nth segmentation 1 in
			let newWord = String.sub sentence startChar (endChar - startChar) in
			if (incrementAmount =/ (num_of_int 1)) then
				begin
					printf "%s" newWord;
					if (List.length segmentation > 2) then
						printf "%s" !wordDelimiter
				end;
			print_segmented_utterance (List.tl segmentation) sentence incrementAmount
		end;;


(* Backs-off from familiar word score to phoneme n-gram score. *)
let default_evidence_combiner word =
	let familiarScore =  (FamiliarWordCue.eval_word word ( */ )) in
	let piecewiseScore = if (!scorePiecewise) then (PhonemePiecewiseCue.eval_word word ( */ )) else (num_of_int 1) in	
	let phonemeScore = if (!phonemeWindow > 0) then (PhonemeNgramCue.eval_word word ( */ )) else badScoreNum in
	let syllableScore = if (!syllableWindow > 0) then (SyllableNgramCue.eval_word word ( */ )) else (num_of_int 0) in
	if (!verbose) then
		begin
			eprintf "Familiar score for %s = %s\nSyllable n-gram score for %s = %s\nPhoneme n-gram score for %s = %s\nPhoneme piecewise score for %s = %s\n\n" word (approx_num_exp 10 familiarScore) word (approx_num_exp 10 syllableScore) word (approx_num_exp 10 phonemeScore) word (approx_num_exp 10 piecewiseScore);
			flush stderr
 		end;
	if (FamiliarWordCue.use_score word) then
		familiarScore
	else
		if (!requireSyllabic && (SyllableNgramCue.use_score word)) || (syllableScore >/ (num_of_int 0)) then (* Can't syllabify word or score is acceptably high. *)
			syllableScore
		else
			phonemeScore */ piecewiseScore;;

let weighted_sum_combiner filterFunctions evalFunctions weights names word =
	let filterScore = List.fold_left (fun currentProduct filterFunc -> (filterFunc word) */ currentProduct) (num_of_int 1) filterFunctions in
	if (filterScore >/ (num_of_int 0)) then
		begin
			let scoreList = List.map (fun evalFunc -> (evalFunc word ( */ ))) evalFunctions in
			if (!verbose) then
				begin
					List.iter2 (fun name score -> eprintf "%s score for %s = %s\n" name word (approx_num_exp 10 score)) names scoreList;
					eprintf "\n"
				end;
			List.fold_left2 (fun currentSum weight score -> currentSum +/ (weight */ score)) (num_of_int 0) weights scoreList
		end
	else
		filterScore;;

let filter_functions = [(fun word -> 
							if (!requireSyllabic) && (SyllableNgramCue.use_score word) then
								(num_of_int 0)
							else
								(num_of_int 1)
						)];;

let eval_functions = [(if (not !noLexicon) then 
							(fun word combine -> 
								if (FamiliarWordCue.use_score word) then 
									FamiliarWordCue.eval_word word combine 
								else 
									badScoreNum
							)
					   else 
							(fun a b -> badScoreNum));
 					  (if (!phonemeWindow > 0) then PhonemeNgramCue.eval_word else (fun a b -> badScoreNum)) ;
					  (if (!scorePiecewise) then PhonemePiecewiseCue.eval_word else (fun a b -> badScoreNum)) ;
					  (if (!syllableWindow > 0) then SyllableNgramCue.eval_word else (fun a b -> badScoreNum)) ;
					  (if (!featureWindow > 0) then FeatureNgramCue.eval_word else (fun a b -> badScoreNum))];;

let eval_weights = [(num_of_float 0.2) ; (num_of_float 0.2) ; (num_of_float 0.2) ; (num_of_float 0.2) ; (num_of_float 0.2)];;

let eval_names = ["Familiar" ; "Phoneme N-gram" ; "Phoneme Piecewise" ; "Syllable" ; "Feature"];;


let eval_word = if (!weightedSum) then 
					(weighted_sum_combiner filter_functions eval_functions eval_weights eval_names)
				else
					default_evidence_combiner;;

let rec mbdp_inner subUtterance firstChar lastChar bestList =
	if firstChar <= lastChar then
		begin
			let newSubUtterance = String.sub subUtterance firstChar ((lastChar + 1) - firstChar) in
			FamiliarWordCue.update_subseq_count newSubUtterance (num_of_int 1);			
			let wordScore = eval_word newSubUtterance in
			let oldBestProduct = fst (bestList.(firstChar - 1)) in
			let lastCharBestProduct = fst (bestList.(lastChar)) in
			let scoreProduct = wordScore */ oldBestProduct in
			if scoreProduct >/ lastCharBestProduct then
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
			(* eprintf "LastChar: %i\tString length: %i\n" lastChar (String.length sentence); *)
			let subUtterance = String.sub sentence 0 (lastChar + 1) in
			FamiliarWordCue.update_subseq_count subUtterance (num_of_int 1);						
			let newBestList = Array.append oldBestList [|((eval_word subUtterance), 0)|] in
			mbdp_inner subUtterance 1 lastChar newBestList
		)
		[||]
		lastCharList
	in
	(* Array.iter (fun (x, y) -> eprintf "(%F, %d)" x y) bestList; *)
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
	[|((num_of_int 1), ((List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence]))|];;

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
	subsequence_updater_outer sentence;
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
											[|(num_of_int 1), (fst (segmentation_of_segmented_sentence segmentedSentence))|]
										else 
											(if (utteranceCount + 1 > !waitUntilUtterance) && ((not !waitForStablePhonemeDist) || (PhonemeNgramCue.use_score sentence)) then
												get_scored_segmentation_list sentence
											else
												[|(num_of_int 1), (fst (segmentation_of_segmented_sentence sentence))|])) in
					if (!displayLineNumbers) then
						fprintf !currentOutputChannel "%d: " (utteranceCount + 1);
					if ((!supervisedFor = 0) || (!supervisedFor > utteranceCount) || (!semisupervisedUpdating)) then
						Array.iter (fun (incrementAmount, segmentation) -> 
											lexicon_updater 
												segmentation 
												sentence 
												(if (not !uniformPhonotactics) then
													[PhonemePiecewiseCue.update_evidence; PhonemeNgramCue.update_evidence; SyllableNgramCue.update_evidence; FamiliarWordCue.update_evidence]
												else
													[FamiliarWordCue.update_evidence])
												incrementAmount
										) 
										segmentations
					else
						Array.iter (fun (incrementAmount, segmentation) -> 
											print_segmented_utterance 
												segmentation 
												sentence
												incrementAmount
										) 
										segmentations;
					if (!printUtteranceDelimiter) then
						fprintf !currentOutputChannel "%s" !utteranceDelimiter;		
					fprintf !currentOutputChannel "\n";
					flush !currentOutputChannel;
					Hashtbl.replace lexicon !utteranceDelimiter (succ_num (Hashtbl.find lexicon !utteranceDelimiter))
				end
		)
		utteranceList;;


(* Learn phonotactic model from gold corpus, and then resegment without updating phonotactic model *)
let two_pass_processor utteranceList = 
	eprintf "\nPhonotactic Pass:\n";
	currentOutputChannel := stderr;
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit = 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let (segmentation, sentence) = segmentation_of_segmented_sentence segmentedSentence in
					lexicon_updater segmentation sentence [PhonemePiecewiseCue.update_evidence; PhonemeNgramCue.update_evidence; SyllableNgramCue.update_evidence; FamiliarWordCue.update_evidence] (num_of_int 1); 
					fprintf !currentOutputChannel "\n";
					flush !currentOutputChannel;
					Hashtbl.replace lexicon !utteranceDelimiter (succ_num (Hashtbl.find lexicon !utteranceDelimiter))
				end
		)
		utteranceList;
	Hashtbl.clear lexicon;
	Hashtbl.add lexicon !utteranceDelimiter (num_of_int 0);
	totalWords := (num_of_int 0);
	eprintf "\nSegmentation Pass:\n";
	flush stderr;
	currentOutputChannel := stdout;
	List.iteri
		(fun utteranceCount segmentedSentence -> 
			if ((!utteranceLimit = 0) || (utteranceCount < !utteranceLimit)) then
				begin
					let sentence = replace ~rex:removeDelimitersPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
					let bestStartList = eval_utterance sentence in
					let segmentation = (List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence] in
					if (!displayLineNumbers) then
						fprintf !currentOutputChannel "%d: " (utteranceCount + 1);
					lexicon_updater segmentation sentence [FamiliarWordCue.update_evidence] (num_of_int 1); 
					if (!printUtteranceDelimiter) then
						fprintf !currentOutputChannel "%s" !utteranceDelimiter;		
					fprintf !currentOutputChannel "\n";
					flush !currentOutputChannel;
					Hashtbl.replace lexicon !utteranceDelimiter (succ_num (Hashtbl.find lexicon !utteranceDelimiter))
				end
		)
		utteranceList;;


let sentence_processor = (if not !goldPhonotactics then
								incremental_processor
							else
								two_pass_processor);;

(*** ACTUAL START OF PROGRAM ***)
if (!interactive) then
	begin
		printf "Utterance number to process to: ";
		utteranceLimit := read_int ();
		utteranceLimit := (if (!utteranceLimit = 0) then 
								-1 
							else
								!utteranceLimit)
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
				  "syllabify" :: args -> 
					List.iter 
						(fun arg -> 
							printf "%s%s" (syllabify arg) !wordDelimiter
						) 
						args; 
					printf "\n"
				| "score" :: args -> printf "Total = %s" (approx_num_exp 10 (List.fold_left (fun acc x -> printf "chosen score: %s\n" (approx_num_exp 10 x); acc */ x) (num_of_int 1) (List.map eval_word args))); printf "\n"
				| "pairs" :: args -> 
					List.iter 
						(fun arg -> 
							List.iter 
							(fun pair -> 
								printf "%s\n" pair
							) 
							(get_pairs (!wordDelimiter ^ arg ^ !wordDelimiter)); 
							printf "\n"
						) 
						args
				| "add" :: incrementAmount :: args -> let (segmentation, sentence) = segmentation_of_word_list args in lexicon_updater segmentation sentence [PhonemeNgramCue.update_evidence; SyllableNgramCue.update_evidence; FamiliarWordCue.update_evidence] (num_of_float (float_of_string incrementAmount)); ()
 				| "help" :: [] -> printf "Available commands: \n    add INCREMENT-AMOUNT WORDS\tincreases the frequencies of WORDS by INCREMENT-AMOUNT\n    score WORDS\t\t\treturns the scores for WORDS\n    syllabify WORDS\t\tbreaks WORDS up into syllables\n    pairs WORDS\t\tprints precedence pairs in WORDS\n"
				| _ -> printf "Unknown command.\n"
			with e ->
				eof := true;
			printf "\n"
		done
	end;;


(* Dump lexicon if requested *)
if !lexiconOut <> "" then
	FamiliarWordCue.dump !lexiconOut;;
	
(* Dump n-gram counts if requested *)
if !phonemeCountsOut <> "" then
	PhonemeNgramCue.dump !phonemeCountsOut;;

(* Dump piecewise counts if requested *)
if !piecewiseCountsOut <> "" then
	PhonemePiecewiseCue.dump !piecewiseCountsOut;;


(* Dump n-gram counts if requested *)
if !featureCountsOut <> "" then
	FeatureNgramCue.dump !featureCountsOut;;

(* Dump n-gram counts if requested *)
if !syllableCountsOut <> "" then
	SyllableNgramCue.dump !syllableCountsOut;;
