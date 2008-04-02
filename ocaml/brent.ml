(* Dan Blanchard
   MBDP Implementation *)

open Pcre
open Printf
open ExtList

let wordDelimiter = ' '
let utteranceDelimiter = "$"
let corpus = "/Users/dan/Documents/Grad School/Research/Segmentation/Implementation/corpora/brent.txt"
let sentenceList = ref []
let lexicon = Hashtbl.create 10000
let phonemeCounts = Hashtbl.create 10000
let wordPhonemeCounts = Hashtbl.create 100
let piSquared = 3.1415926536 ** 2.0
let removeSpacesPattern = regexp "((\\s)|(\\.))+"
let totalWords = ref 0
let totalPhonemes = ref 0;;

let hashstrint_print = Hashtbl.iter (fun key data -> printf "%s: %d; " key data);;
let hashchart_print = Hashtbl.iter (fun key data -> printf "%c: %d; " key data);;

Hashtbl.add lexicon utteranceDelimiter 0;;
Hashtbl.add phonemeCounts wordDelimiter 0;;

(* Calculates the probability of each phoneme in a word*)
let prob_phonemes word wordPhonemeCounts wordTotalPhonemes =
	let wordWithBoundary = word ^ String.make 1 wordDelimiter in
	let wordTotalPhonemesFloat = float wordTotalPhonemes in
	let phonemeScore = ref (1.0 /. (1.0 -. ((float (Hashtbl.find wordPhonemeCounts wordDelimiter)) /. wordTotalPhonemesFloat))) in
	String.iter (* Get adjusted phoneme counts *)
		(fun phoneme ->
			if Hashtbl.mem wordPhonemeCounts phoneme then
				phonemeScore := !phonemeScore *. ((float (Hashtbl.find wordPhonemeCounts phoneme)) /. wordTotalPhonemesFloat)
			else
				phonemeScore := !phonemeScore *. ((float (Hashtbl.find phonemeCounts phoneme)) /. wordTotalPhonemesFloat)
		)
		wordWithBoundary;
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
			let wordWithBoundary = word ^ String.make 1 wordDelimiter in
			String.iter (* Get adjusted phoneme counts *)
				(fun phoneme ->
					if Hashtbl.mem wordPhonemeCounts phoneme then
						Hashtbl.replace wordPhonemeCounts phoneme ((Hashtbl.find wordPhonemeCounts phoneme) + 1)
					else if Hashtbl.mem phonemeCounts phoneme then
						Hashtbl.add wordPhonemeCounts phoneme ((Hashtbl.find phonemeCounts phoneme) + 1)
					else
						Hashtbl.add wordPhonemeCounts phoneme 1
				)
				wordWithBoundary;
			wordTotalPhonemes := !totalPhonemes + String.length wordWithBoundary;
			score := (6.0 /. piSquared) *. (wordTypesFloat /. (totalWordsFloat +. 1.0));
			let wordPhonemeScore = prob_phonemes word wordPhonemeCounts !wordTotalPhonemes in
			score := !score *. (wordPhonemeScore /. (1.0 -. ((wordTypesFloat -. 1.0) /. wordTypesFloat) *. wordPhonemeScore));
			score := !score *. ((wordTypesFloat -. 1.0) /. wordTypesFloat) ** 2.0;
		end;
	(* printf "\nScore for %s = %e\n" word !score; *)
	!score;;

 let mbdp_imperative sentence =
	let bestProduct = DynArray.make ((String.length sentence) - 1) in
	let bestStart = DynArray.make ((String.length sentence) - 1) in
	for lastChar = 0 to ((String.length sentence) - 1) do
		DynArray.add bestProduct (r (String.sub sentence 0 (lastChar + 1)));
		DynArray.add bestStart 0;
		for firstChar = 1 to lastChar do			
			let wordScore = r (String.sub sentence firstChar ((lastChar + 1) - firstChar)) in
			let scoreProduct = wordScore *. (DynArray.get bestProduct (firstChar - 1)) in
			if scoreProduct > (DynArray.get bestProduct lastChar) then
				begin
					DynArray.set bestProduct lastChar scoreProduct;
					DynArray.set bestStart lastChar firstChar
				end
			else
				()				
		done
	done;
	DynArray.to_list bestStart;; 

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
	let lastCharList = List.init ((String.length sentence) - 1) (fun a -> a) in
	let bestList = List.fold_left
		(fun oldBestList lastChar ->
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
			(* printf "startChar = %d\tendChar =%d\n" startChar endChar; *)
			printf "%s%c" newWord wordDelimiter;
			totalWords := !totalWords + 1;
			totalPhonemes := !totalPhonemes + (String.length newWord) + 1; (* Add one for the delimiter*)		
			Hashtbl.replace phonemeCounts wordDelimiter ((Hashtbl.find phonemeCounts wordDelimiter) + 1);
			String.iter 
				(fun phoneme ->
					if Hashtbl.mem phonemeCounts phoneme then
						Hashtbl.replace phonemeCounts phoneme ((Hashtbl.find phonemeCounts phoneme) + 1)
					else
						Hashtbl.add phonemeCounts phoneme 1		
				)
				newWord;
			if Hashtbl.mem lexicon newWord then
				Hashtbl.replace lexicon newWord ((Hashtbl.find lexicon newWord) + 1)
			else
				Hashtbl.add lexicon newWord 1;
			lexicon_updater (List.tl segmentation) sentence
		end
	else
		();;

(* Read file *)
let ic = open_in corpus in
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
		(* let bestStartList = mbdp_imperative sentence in *)
		let segmentation = (List.sort (find_segmentation bestStartList (List.length bestStartList) [])) @ [String.length sentence] in
		(* printf "\nLexicon = %s" "";
		hashstrint_print lexicon;
		printf "\nPhoneme counts = %s" "";
		hashchart_print phonemeCounts; *)
		(* printf "bestStartList = %s" "";
		List.iter (fun x -> printf "%d\t" x) bestStartList; *)
		(* printf "\nsegmentation = %s" "";
		List.iter (fun x -> printf "%d\t" x) segmentation; *)
		printf "\n%d: " ((Hashtbl.find lexicon utteranceDelimiter) + 1);
		lexicon_updater segmentation sentence; 
		printf "%s\n" utteranceDelimiter;
		Hashtbl.replace lexicon utteranceDelimiter ((Hashtbl.find lexicon utteranceDelimiter) + 1)
	)
	!sentenceList;;
