(* Dan Blanchard
   MBDP Implementation *)

open Pcre
open Printf
open ExtList

let displayLineNumbers = ref false
let badScore =  ref 0.000000000000001
let wordDelimiter = ref " "
let utteranceDelimiter = ref "$"
let corpus = ref "/Users/dan/Documents/Grad School/Research/Segmentation/Implementation/corpora/brent.txt"
let sentenceList = ref []
let lexiconOut = ref ""
let phonemeCountsOut = ref ""
let lexicon = Hashtbl.create 10000
let phonemeCounts = Hashtbl.create 10000
let wordPhonemeCounts = Hashtbl.create 100
let sixOverPiSquared = 6.0 /. (3.1415926536 ** 2.0)
let removeSpacesPattern = regexp "((\\s)|(\\.))+"
let windowSize = ref 1
let totalWords = ref 0
let totalPhonemes = ref 0;;

let hash_print = Hashtbl.iter (fun key data -> printf "%s: %d; " key data);;
let hash_fprint file = Hashtbl.iter (fun key data -> fprintf file "%s\t%d; " key data);;

(* Process command-line arguments *)
let process_anon_args corpusFile = corpus := corpusFile;;
let arg_spec_list =["--wordDelimiter", Arg.Set_string wordDelimiter, " Word delimiter";
					"-wd", Arg.Set_string wordDelimiter, " Short for --wordDelimiter";
					"--utteranceDelimiter", Arg.Set_string utteranceDelimiter, " Utterance delimiter"; 
					"-ud", Arg.Set_string utteranceDelimiter, " Short for --utteranceDelimiter"; 
					"--windowSize", Arg.Set_int windowSize, " Window size for n-grams";
					"-ws", Arg.Set_int windowSize, " Short for --windowSize";
					"--badScore", Arg.Set_float badScore, " Score assigned when word length is less than window size";
					"-bs", Arg.Set_float badScore, " Short for --badScore";
					"--lineNumbers", Arg.Set displayLineNumbers, " Display line numbers before each segmented utterance";
					"-ln", Arg.Set displayLineNumbers, " Short for --lineNumbers";
					"--lexiconOut", Arg.Set_string lexiconOut, " File to dump final lexicon to";
					"-lo", Arg.Set_string lexiconOut, " Short for --lexiconOut";
					"--ngramsOut", Arg.Set_string lexiconOut, " File to dump final n-gram counts to";
					"-no", Arg.Set_string lexiconOut, " Short for --ngramsOut"];;
let usage = Sys.executable_name ^ " [-options]";;
Arg.parse arg_spec_list	process_anon_args usage;;

(* Setup initial values for utteranceDelimiter and wordDelimiter in the lexicon and phonemeCounts*)
Hashtbl.add lexicon !utteranceDelimiter 0;;
Hashtbl.add phonemeCounts !wordDelimiter 0;;

(* Calculates the probability of each phoneme in a word*)
let prob_phonemes word wordPhonemeCounts wordTotalPhonemes =
	let wordWithBoundary = (if !windowSize > 1 then 
								!wordDelimiter ^ word ^ !wordDelimiter 
							else 
								word ^ !wordDelimiter) in
	if (String.length word) < !windowSize then
		!badScore
	else		
		let wordTotalPhonemesFloat = float wordTotalPhonemes in
		let phonemeScore = ref (if !windowSize > 1 then 
									1.0
								else
									(1.0 /. (1.0 -. ((float (Hashtbl.find wordPhonemeCounts !wordDelimiter)) /. wordTotalPhonemesFloat)))) in
		let firstCharList = List.init ((String.length wordWithBoundary) - (!windowSize - 1)) (fun a -> a) in
		List.iter (* Get adjusted phoneme counts *)
			(fun firstChar ->
				let phoneme = String.sub wordWithBoundary firstChar !windowSize in
				if Hashtbl.mem wordPhonemeCounts phoneme then
					phonemeScore := !phonemeScore *. ((float (Hashtbl.find wordPhonemeCounts phoneme)) /. wordTotalPhonemesFloat)
				else
					phonemeScore := !phonemeScore *. ((float (Hashtbl.find phonemeCounts phoneme)) /. wordTotalPhonemesFloat)
			)
			firstCharList;
		!phonemeScore;;


(* Function to calculate r-score*)
let r word =
	Hashtbl.clear wordPhonemeCounts;
	let wordTotalPhonemes = ref 0 in
	let wordTypesFloat = float (Hashtbl.length lexicon) in
	let totalWordsFloat = float !totalWords in
	let score = ref 0.0 in
	if Hashtbl.mem lexicon word then 	(* familiar word*)
		begin
			let wordCountFloat = float (Hashtbl.find lexicon word) in
			score := ((wordCountFloat +. 1.0) /. (totalWordsFloat +. 1.0)) *. (((wordCountFloat) /. (wordCountFloat +. 1.0)) ** 2.0)
		end
	else	(* novel word *)
		begin
			let wordWithBoundary = (if !windowSize > 1 then 
										!wordDelimiter ^ word ^ !wordDelimiter 
									else 
										word ^ !wordDelimiter) in
			if (String.length wordWithBoundary) < !windowSize then
				score := !badScore
			else												
				let firstCharList = List.init ((String.length wordWithBoundary) - (!windowSize - 1)) (fun a -> a) in
				List.iter (* Get adjusted phoneme counts *)
					(fun firstChar ->
						let phoneme = String.sub wordWithBoundary firstChar !windowSize in
						if Hashtbl.mem wordPhonemeCounts phoneme then
							Hashtbl.replace wordPhonemeCounts phoneme ((Hashtbl.find wordPhonemeCounts phoneme) + 1)
						else if Hashtbl.mem phonemeCounts phoneme then
							Hashtbl.add wordPhonemeCounts phoneme ((Hashtbl.find phonemeCounts phoneme) + 1)
						else
							Hashtbl.add wordPhonemeCounts phoneme 1;
					)
					firstCharList;
				wordTotalPhonemes := !totalPhonemes + String.length wordWithBoundary;
				score := sixOverPiSquared *. (wordTypesFloat /. (totalWordsFloat +. 1.0));
				let wordPhonemeScore = prob_phonemes word wordPhonemeCounts !wordTotalPhonemes in
				score := !score *. (wordPhonemeScore /. (1.0 -. ((wordTypesFloat -. 1.0) /. wordTypesFloat) *. wordPhonemeScore));
				score := !score *. ((wordTypesFloat -. 1.0) /. wordTypesFloat) ** 2.0;
		end;
	(* printf "\nScore for %s = %e\n" word !score; *)
	!score;;

let rec mbdp_inner subUtterance firstChar lastChar bestList =
	if firstChar <= lastChar then
		begin
			let newSubUtterance = String.sub subUtterance firstChar ((lastChar + 1) - firstChar) in
			let wordScore = r newSubUtterance in
			let oldBestProduct = fst (List.nth bestList (firstChar - 1)) in
			let lastCharBestProduct = fst (List.nth bestList lastChar) in
			let scoreProduct = wordScore *. oldBestProduct in
			if scoreProduct > lastCharBestProduct then
				mbdp_inner subUtterance (firstChar + 1) lastChar ((List.take lastChar bestList) @ [(scoreProduct, firstChar)] @ (List.drop (lastChar + 1) bestList))
			else	
				mbdp_inner subUtterance (firstChar + 1) lastChar bestList
		end
	else
		bestList;;

let mbdp_outer sentence =
	let lastCharList = List.init (String.length sentence) (fun a -> a) in
	let bestList = List.fold_left
		(fun oldBestList lastChar ->
			(* printf "LastChar: %i\tString length: %i\n" lastChar (String.length sentence); *)
			let subUtterance = String.sub sentence 0 (lastChar + 1) in
			let newBestList = oldBestList @ [((r subUtterance), 0)] in
			mbdp_inner subUtterance 1 lastChar newBestList
		)
		[]
		lastCharList
	in
	(* List.iter (fun (x, y) -> printf "(%e, %d)" x y) bestList; *)
	List.map
	 	snd
		bestList;;

(* Finds the path through bestStartList which reveals the starts and stops of all hypothesized words in the utterance.
	Make sure you call this with firstChar set to the length of the utterance*)
let rec find_segmentation bestStartList firstChar path =
	if firstChar > 0 then
		begin
			let newFirstChar = List.nth bestStartList (firstChar - 1) in
			find_segmentation bestStartList newFirstChar (path @ [newFirstChar])
		end
	else
		path;;
		
let rec lexicon_updater segmentation sentence =
	if (List.length segmentation) > 1 then
		begin
			let startChar = List.nth segmentation 0 in
			let endChar = List.nth segmentation 1 in
			let newWord = String.sub sentence startChar (endChar - startChar) in
			let wordWithBoundary = (if !windowSize > 1 then 
										!wordDelimiter ^ newWord ^ !wordDelimiter 
									else 
										newWord ^ !wordDelimiter) in
			let wordWindow = (if (String.length newWord) < !windowSize then
								String.length newWord
							else
								!windowSize) in
			(* printf "startChar = %d\tendChar =%d\n" startChar endChar; *)
			printf "%s" (newWord ^ !wordDelimiter);
			totalWords := !totalWords + 1;
			totalPhonemes := !totalPhonemes + (String.length wordWithBoundary);
			let firstCharList = List.init ((String.length wordWithBoundary) - (wordWindow - 1)) (fun a -> a) in
			List.iter (* Get adjusted phoneme counts *)
				(fun firstChar ->
					let phoneme = String.sub wordWithBoundary firstChar wordWindow in
					if Hashtbl.mem phonemeCounts phoneme then
						Hashtbl.replace phonemeCounts phoneme ((Hashtbl.find phonemeCounts phoneme) + 1)
					else
						Hashtbl.add phonemeCounts phoneme 1		
				)
				firstCharList;
			if Hashtbl.mem lexicon newWord then
				Hashtbl.replace lexicon newWord ((Hashtbl.find lexicon newWord) + 1)
			else
				Hashtbl.add lexicon newWord 1;
			lexicon_updater (List.tl segmentation) sentence
		end
	else
		();;

(* Read corpus file *)
let ic = open_in !corpus in
try
	sentenceList := Std.input_list ic;
	close_in ic
with e ->
	close_in_noerr ic;
	raise e;;

(* Loop through sentences *)
List.iter
	(fun segmentedSentence -> 
		let sentence = replace ~rex:removeSpacesPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
		let bestStartList = mbdp_outer sentence in
		let segmentation = (List.sort (find_segmentation bestStartList (List.length bestStartList) [])) @ [String.length sentence] in
		(* printf "\nLexicon = %s" "";
		hashstrint_print lexicon;
		printf "\nPhoneme counts = %s" "";
		hashchart_print phonemeCounts; *)
		(* printf "bestStartList = %s" "";
		List.iter (fun x -> printf "%d\t" x) bestStartList; *)
		(* printf "\nsegmentation = %s" "";
		List.iter (fun x -> printf "%d\t" x) segmentation; *)
		if (!displayLineNumbers) then
			printf "%d: " ((Hashtbl.find lexicon !utteranceDelimiter) + 1);
		lexicon_updater segmentation sentence; 
		printf "%s\n" !utteranceDelimiter;
		Hashtbl.replace lexicon !utteranceDelimiter ((Hashtbl.find lexicon !utteranceDelimiter) + 1)
	)
	!sentenceList;;

(* Dump lexicon if requested *)
if !lexiconOut <> "" then
	let oc = open_out !lexiconOut in
	hash_fprint oc lexicon;
	close_out oc;;

(* Dump n-gram counts if requested *)
if !phonemeCountsOut <> "" then
	let oc = open_out !phonemeCountsOut in
	hash_fprint oc phonemeCounts;
	close_out oc;;
