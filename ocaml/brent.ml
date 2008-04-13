(* Dan Blanchard
   MBDP Implementation *)

open Pcre
open Printf
open ExtList
open ExtString
open ExtArray

(* SHOULD BE IN ITS OWN MODULE - HACK *)
let phonesToFeatures = Hashtbl.create 100
let featuresToPhones = Hashtbl.create 100
let features = ref []
(**************************************)

let printUtteranceDelimiter = ref false
let displayLineNumbers = ref false
let featureFile = ref ""
let badScore =  ref 0.0
let wordDelimiter = ref " "
let utteranceDelimiter = ref "$"
let corpus = ref ""
let sentenceList = ref []
let lexiconOut = ref ""
let phonemeCountsOut = ref ""
let lexicon = Hashtbl.create 10000
let phonemeCounts = Hashtbl.create 10000
let wordPhonemeCounts = Hashtbl.create 100
let cartesianProductCache = Hashtbl.create 10000
let sixOverPiSquared = 6.0 /. (3.1415926536 ** 2.0)
let removeSpacesPattern = regexp "((\\s)|(\\.))+"
let windowSize = ref 1
let totalWords = ref 0
let totalPhonemes = ref 0;;

module StringSet = Set.Make(String);;

let hash_print = Hashtbl.iter (fun key data -> printf "%s: %d;" key data);;
(* let hash_dump = Hashtbl.iter (fun key data -> Std.print key; Std.print data);; *)
let hash_fprint file = Hashtbl.iter (fun key data -> fprintf file "%s\t%d\n" key data);;

(* Process command-line arguments *)
let process_anon_args corpusFile = corpus := corpusFile;;
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
					"--lineNumbers", Arg.Set displayLineNumbers, " Display line numbers before each segmented utterance";
					"-ln", Arg.Set displayLineNumbers, " Short for --lineNumbers";
					"--printUtteranceDelimiter", Arg.Set printUtteranceDelimiter, " Print utterance ";
					"-pu", Arg.Set printUtteranceDelimiter, " Short for --printUtteranceDelimiter";
					"--lexiconOut", Arg.Set_string lexiconOut, " File to dump final lexicon to";
					"-lo", Arg.Set_string lexiconOut, " Short for --lexiconOut";
					"--ngramsOut", Arg.Set_string phonemeCountsOut, " File to dump final n-gram counts to";
					"-no", Arg.Set_string phonemeCountsOut, " Short for --ngramsOut"];;
let usage = Sys.executable_name ^ " [-options] CORPUS";;
Arg.parse arg_spec_list	process_anon_args usage;;

(* Setup initial value for utteranceDelimiter in the lexicon *)
Hashtbl.add lexicon !utteranceDelimiter 0;;

(************ SHOULD BE IN ITS OWN MODULE: FEATURE CHART **********)
(* Read feature file *)
let read_feature_file featureFile =
	let ic = open_in featureFile in
	try
		let fileLines = Std.input_list ic in
		close_in ic;
		(* Fill feature list *)
		List.iter 
			(fun feature ->	
				features := !features @ [feature]
			)
			(String.nsplit (String.lchop (List.hd fileLines)) "\t");
		(* Fill phonesToFeatures and featuresToPhones tables*)
		List.iter
			(fun line ->
				let lineList = String.nsplit line "\t" in
				let phone = List.hd lineList in
				let featureList = List.tl lineList in
				let currentFeatureSet = (List.fold_left
														(fun oldFeatureSet newFeature ->
															if newFeature <> "" then
																StringSet.add newFeature oldFeatureSet
															else
																oldFeatureSet
														)
														StringSet.empty
														(List.mapi
															(fun index value ->
																if value <> "0" then
																	let featureValue = value ^ (List.nth !features index) in
																	if Hashtbl.mem featuresToPhones featureValue then
																		Hashtbl.replace featuresToPhones featureValue (StringSet.add phone (Hashtbl.find featuresToPhones featureValue))
																	else
																		Hashtbl.add featuresToPhones featureValue (StringSet.add phone StringSet.empty);
																	featureValue
																else
																	""
															)
															featureList)) in
				Hashtbl.add phonesToFeatures phone currentFeatureSet
			)
			(List.tl fileLines);
	with e ->
		close_in_noerr ic;
		raise e;;

let features_for_phone phone =
	Hashtbl.find phonesToFeatures phone;;

let print_string_set stringSet = 
	StringSet.iter
		(fun currentFeature -> 
			printf "%s\t" currentFeature
		)
		stringSet;
	printf "\n";;
(*******************END FEATURE CHART ************************)




(* Calculates the probability of each phoneme in a word*)
let prob_phonemes word wordPhonemeCounts wordTotalPhonemes =
	let wordWithBoundary = (if !windowSize > 1 then 
								!wordDelimiter ^ word ^ !wordDelimiter 
							else 
								word ^ !wordDelimiter) in							
	if (String.length word) < !windowSize then
		-. (log !badScore)
	else	
		let wordTotalPhonemesFloat = float wordTotalPhonemes in			
		let phonemeScore = ref (if !windowSize > 1 then 
									-.(log 1.0)
								else
									-.(log (1.0 /. (1.0 -. ((float (Hashtbl.find wordPhonemeCounts !wordDelimiter)) /. wordTotalPhonemesFloat))))) in
		let firstCharList = List.init ((String.length wordWithBoundary) - (!windowSize - 1)) (fun a -> a) in
		if !featureFile <> "" then
			begin
				let firstCharListForBundles = Array.init (String.length wordWithBoundary) (fun a -> a) in
				let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
										(fun firstChar ->
											let phoneme = String.sub wordWithBoundary firstChar 1 in
											features_for_phone phoneme											
										) 
										firstCharListForBundles 
				in
				List.iter
					(fun firstChar ->
						let ngramFeatures = Array.sub wordFeatures firstChar !windowSize in
						let lastCharList = List.init (!windowSize - 1) (fun a -> (a + 1)) in
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
								if Hashtbl.mem wordPhonemeCounts featureGram then
									phonemeScore := !phonemeScore -. (log ((float (Hashtbl.find wordPhonemeCounts featureGram)) /. wordTotalPhonemesFloat))
								else
									phonemeScore := !phonemeScore -. (log ((float (Hashtbl.find phonemeCounts featureGram)) /. wordTotalPhonemesFloat))
							)
							ngramFeatureSet
					)
					firstCharList
			end
		else
			begin
				List.iter (* Get adjusted phoneme counts *)
					(fun firstChar ->
						let phoneme = String.sub wordWithBoundary firstChar !windowSize in
						if Hashtbl.mem wordPhonemeCounts phoneme then
							phonemeScore := !phonemeScore -. (log ((float (Hashtbl.find wordPhonemeCounts phoneme)) /. wordTotalPhonemesFloat))
						else
							phonemeScore := !phonemeScore -. (log ((float (Hashtbl.find phonemeCounts phoneme)) /. wordTotalPhonemesFloat))
					)
					firstCharList;
			end;
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
			score := -.(log (((wordCountFloat +. 1.0) /. (totalWordsFloat +. 1.0)) *. (((wordCountFloat) /. (wordCountFloat +. 1.0)) ** 2.0)))
		end
	else	(* novel word *)
		begin
			let wordWithBoundary = (if !windowSize > 1 then 
										!wordDelimiter ^ word ^ !wordDelimiter 
									else 
										word ^ !wordDelimiter) in
			if (String.length wordWithBoundary) < !windowSize then
				score := -. (log (!badScore))
			else												
				let firstCharList = List.init ((String.length wordWithBoundary) - (!windowSize - 1)) (fun a -> a) in
				if !featureFile <> "" then
					begin
						let firstCharListForBundles = Array.init (String.length wordWithBoundary) (fun a -> a) in
						let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
												(fun firstChar ->
													let phoneme = String.sub wordWithBoundary firstChar 1 in
													features_for_phone phoneme
												) 
												firstCharListForBundles 
						in
						List.iter
							(fun firstChar ->
								let ngramFeatures = Array.sub wordFeatures firstChar !windowSize in
								let lastCharList = List.init (!windowSize - 1) (fun a -> (a + 1)) in
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
										if Hashtbl.mem wordPhonemeCounts featureGram then
											Hashtbl.replace wordPhonemeCounts featureGram ((Hashtbl.find wordPhonemeCounts featureGram) + 1)
										else if Hashtbl.mem phonemeCounts featureGram then
											Hashtbl.add wordPhonemeCounts featureGram ((Hashtbl.find phonemeCounts featureGram) + 1)
										else
											Hashtbl.add wordPhonemeCounts featureGram 1;
										totalPhonemes := !totalPhonemes + 1									
									)
									ngramFeatureSet
							)
							firstCharList
					end
				else
					begin
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
					end;
				score := -.(log (sixOverPiSquared *. (wordTypesFloat /. (totalWordsFloat +. 1.0))));
				let wordPhonemeScore = prob_phonemes word wordPhonemeCounts !wordTotalPhonemes in
				score := !score +. wordPhonemeScore;
				score := !score -. log (((wordTypesFloat -. 1.0) /. wordTypesFloat) ** 2.0);
		end;
	(* printf "\nScore for %s = %e\n" word !score; *)
	!score;;

let rec mbdp_inner subUtterance firstChar lastChar bestList =
	if firstChar <= lastChar then
		begin
			let newSubUtterance = String.sub subUtterance firstChar ((lastChar + 1) - firstChar) in
			let wordScore = r newSubUtterance in
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
			let newBestList = Array.append oldBestList [|((r subUtterance), 0)|] in
			mbdp_inner subUtterance 1 lastChar newBestList
		)
		[||]
		lastCharList
	in
	(* List.iter (fun (x, y) -> printf "(%e, %d)" x y) bestList; *)
	Array.map
	 	snd
		bestList;;

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
		
(* Updates lexicon with newly segmented words, and prints out segmented utterance *)
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
			let wordWindow = (if (String.length wordWithBoundary) < !windowSize then
									String.length wordWithBoundary
							else
								!windowSize) 
			in
			(* printf "startChar = %d\tendChar =%d\n" startChar endChar; *)
			printf "%s" (newWord ^ !wordDelimiter);
			totalWords := !totalWords + 1;
			let firstCharList = List.init ((String.length wordWithBoundary) - (wordWindow - 1)) (fun a -> a) in
			if !featureFile <> "" then
				begin
					let firstCharListForBundles = Array.init (String.length wordWithBoundary) (fun a -> a) in
					let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
											(fun firstChar ->
												let phoneme = String.sub wordWithBoundary firstChar 1 in												
												features_for_phone phoneme
											) 
											firstCharListForBundles 
					in
					List.iter
						(fun firstChar ->							
							let ngramFeatures = Array.sub wordFeatures firstChar wordWindow in
							let lastCharList = List.init (wordWindow - 1) (fun a -> (a + 1)) in
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
									if Hashtbl.mem phonemeCounts featureGram then
										Hashtbl.replace phonemeCounts featureGram ((Hashtbl.find phonemeCounts featureGram) + 1)
									else
										Hashtbl.add phonemeCounts featureGram 1;
									totalPhonemes := !totalPhonemes + 1
								)
								ngramFeatureSet;							
						)
						firstCharList;
				end
			else
				begin
					List.iter (* Adjusts n-gram counts *)
						(fun firstChar ->
							let phoneme = String.sub wordWithBoundary firstChar wordWindow in
							if Hashtbl.mem phonemeCounts phoneme then
								Hashtbl.replace phonemeCounts phoneme ((Hashtbl.find phonemeCounts phoneme) + 1)
							else
								Hashtbl.add phonemeCounts phoneme 1;
							totalPhonemes := !totalPhonemes + 1
						)
						firstCharList
				end;
			if Hashtbl.mem lexicon newWord then
				Hashtbl.replace lexicon newWord ((Hashtbl.find lexicon newWord) + 1)
			else
				Hashtbl.add lexicon newWord 1;
			lexicon_updater (List.tl segmentation) sentence
		end
	else
		();;

(* Read corpus file, if specified, otherwise read from stdin *)
if !corpus <> "" then
	let ic = open_in !corpus in
	try
		sentenceList := Std.input_list ic;
		close_in ic
	with e ->
		close_in_noerr ic;
		raise e
else
	(* The prompt below would not print out above the input, so I commented it out.*)
	(* printf "Please enter each unsegmented utterance on its own line.  Terminate entry with ^D.\n\n"; *)
	sentenceList := Std.input_list stdin;
	close_in stdin;;	

(* Read feature file, if specifed *)
if !featureFile <> "" then
	read_feature_file !featureFile;;

(* Loop through sentences *)
List.iter
	(fun segmentedSentence -> 
		let sentence = replace ~rex:removeSpacesPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
		let bestStartList = mbdp_outer sentence in
		let segmentation = (List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence] in
		if (!displayLineNumbers) then
			printf "%d: " ((Hashtbl.find lexicon !utteranceDelimiter) + 1);
		lexicon_updater segmentation sentence; 
		if (!printUtteranceDelimiter) then
			printf "%s" !utteranceDelimiter;		
		printf "\n";
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
	