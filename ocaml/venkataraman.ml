(* Dan Blanchard
   Venkataraman (2001) Implementation *)

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
let cartesianProductCache = Hashtbl.create 10000

let sixOverPiSquared = 6.0 /. (3.1415926536 ** 2.0)
let removeSpacesPattern = regexp "((\\s)|(\\.))+"
let windowSize = ref 1
let condProb = ref true
let smooth = ref false
let totalWords = ref 0;;

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
					"--printUtteranceDelimiter", Arg.Set printUtteranceDelimiter, " Print utterance delimiter at the end of each utterance";
					"-pu", Arg.Set printUtteranceDelimiter, " Short for --printUtteranceDelimiter";
					"--lexiconOut", Arg.Set_string lexiconOut, " File to dump final lexicon to";
					"-lo", Arg.Set_string lexiconOut, " Short for --lexiconOut";
					"--ngramsOut", Arg.Set_string phonemeCountsOut, " File to dump final n-gram counts to";
					"-no", Arg.Set_string phonemeCountsOut, " Short for --ngramsOut";
					"--conditionalProbability", Arg.Set condProb, " Use conditional probabilities instead of joint";
					"-cp", Arg.Set condProb, " Short for --conditionalProbability"];;
let usage = Sys.executable_name ^ " [-options] CORPUS";;
Arg.parse arg_spec_list	process_anon_args usage;;

(* Setup initial value for utteranceDelimiter in the lexicon *)
Hashtbl.add lexicon !utteranceDelimiter 0;;

let ngramCountsArray = Array.init (!windowSize) (fun a -> Hashtbl.create (int_of_float (10.0 ** (float (a + 2)))));;
let totalNgramsArray = Array.init (!windowSize) (fun a -> 0);;
let typesWithCountArray = Array.init 3 (fun a -> 0);;
let ngramList = List.init !windowSize (fun a -> a);; (* Use this for List.Iter to loop through ngram sizes instead of using a for loop *)

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
		(float (Hashtbl.find wordNgramCountsArray.(n) ngram)) /. (float wordTotalNgramsArray.(n))
	else
		begin
			if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
				begin					
					if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find wordNgramCountsArray.(n) ngram)) /. (float (Hashtbl.find wordNgramCountsArray.(n - 1) prefix))
					else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find wordNgramCountsArray.(n) ngram)) /. (float (Hashtbl.find ngramCountsArray.(n - 1) prefix))
					else
						!badScore
				end
			else 
				begin					
					if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find ngramCountsArray.(n) ngram)) /. (float (Hashtbl.find wordNgramCountsArray.(n - 1) prefix))
					else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find ngramCountsArray.(n) ngram)) /. (float (Hashtbl.find ngramCountsArray.(n - 1) prefix))
					else
						!badScore
				end								
		end;;

let prob_ngram_kneser_ney ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray = 
	let prefix = String.sub ngram 0 n in
	let d = discount n in
	if (n = 0) then
		(float (Hashtbl.find wordNgramCountsArray.(n) ngram)) /. (float wordTotalNgramsArray.(n))
	else
		begin
			if (Hashtbl.mem wordNgramCountsArray.(n) ngram) then
				begin					
					if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find wordNgramCountsArray.(n) ngram)) /. (float (Hashtbl.find wordNgramCountsArray.(n - 1) prefix))
					else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find wordNgramCountsArray.(n) ngram)) /. (float (Hashtbl.find ngramCountsArray.(n - 1) prefix))
					else
						!badScore
				end
			else 
				begin					
					if (Hashtbl.mem wordNgramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find ngramCountsArray.(n) ngram)) /. (float (Hashtbl.find wordNgramCountsArray.(n - 1) prefix))
					else if (Hashtbl.mem ngramCountsArray.(n - 1) prefix) then
						(float (Hashtbl.find ngramCountsArray.(n) ngram)) /. (float (Hashtbl.find ngramCountsArray.(n - 1) prefix))
					else
						!badScore
				end								
		end;;
	
(* Computes the probability of an n-gram within a word;  n is actually n - 1 in this function *)
let prob_ngram_joint ngram n wordNgramCountsArray wordTotalNgramsArray =
	(float (Hashtbl.find wordNgramCountsArray.(n) ngram)) /. (float wordTotalNgramsArray.(n));;

let prob_ngram ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray = 
	if (!condProb) then
		begin
			if (!smooth) then
				prob_ngram_kneser_ney ngram n wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray
			else
				prob_ngram_conditional ngram n wordNgramCountsArray wordTotalNgramsArray
		end
	else
		prob_ngram_joint ngram n wordNgramCountsArray wordTotalNgramsArray;;

(* Calculates the probability of each phoneme in a word*)
let prob_phonemes word wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray combine_ngram_score combine_phoneme_score =
	let wordWithBoundary = (if !windowSize > 1 then 
								!wordDelimiter ^ word ^ !wordDelimiter 
							else 
								word ^ !wordDelimiter) in							
	if (String.length word) < !windowSize then
		-. (log !badScore)
	else	
		let phonemeScore = ref (if !windowSize > 1 then 
								-.(log 1.0)
							else
								-.(log (1.0 /. (1.0 -. ((float (Hashtbl.find wordNgramCountsArray.(0) !wordDelimiter)) /. (float wordTotalNgramsArray.(0))))))) in
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
						let ngramScore = ref (-.(log 1.0)) in 
						StringSet.iter
							(fun featureGram ->
								ngramScore := (combine_ngram_score !ngramScore (-. (log (prob_ngram featureGram (!windowSize - 1) wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray))))								
							)
							ngramFeatureSet;
						phonemeScore := (combine_phoneme_score !phonemeScore !ngramScore)
					)
					firstCharList
			end
		else
			begin
				List.iter (* Get ngram scores *)
					(fun firstChar ->
						let ngram = String.sub wordWithBoundary firstChar !windowSize in
						phonemeScore := (combine_phoneme_score !phonemeScore (-. (log (prob_ngram ngram (!windowSize - 1) wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray))))
					)
					firstCharList;
			end;
	!phonemeScore;;
						
(* Function to calculate r-score*)
let evalWord word =
	let wordNgramCountsArray = Array.init (!windowSize) (fun a -> Hashtbl.create 100) in
	let wordTotalNgramsArray = Array.init (!windowSize) (fun a -> 0) in
	let wordTypesWithCountArray = Array.init 3 (fun a -> typesWithCountArray.(a)) in
	let wordTypesFloat = float (Hashtbl.length lexicon) in
	let totalWordsFloat = float !totalWords in
	let score = ref 0.0 in
	if Hashtbl.mem lexicon word then 	(* familiar word*)
		begin
			let wordCountFloat = float (Hashtbl.find lexicon word) in		
			score := -.(log (wordCountFloat /. (totalWordsFloat +. wordTypesFloat)))
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
				if !featureFile <> "" then
					begin
						List.iter (* Get feature n-gram counts of all size *)
							(fun currentWindowSizeMinusOne ->						
								let firstCharListForBundles = Array.init (String.length wordWithBoundary) (fun a -> a) in
								let firstCharList = List.init ((String.length wordWithBoundary) - currentWindowSizeMinusOne) (fun a -> a) in
								let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
														(fun firstChar ->
															let phoneme = String.sub wordWithBoundary firstChar 1 in
															features_for_phone phoneme
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
													Hashtbl.replace wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram) + 1)
												else if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) featureGram then
													Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) featureGram) + 1)
												else
													Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) featureGram 1;
												Array.set wordTotalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) + 1)							
											)
											ngramFeatureSet;
									)
									firstCharList
							)
							ngramList
					end
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
											Hashtbl.add wordNgramCountsArray.(currentWindowSizeMinusOne) ngram 0;
									)
									ngramFirstCharList;
								Array.set wordTotalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) + ngramFirstCharListLength)
							)
							ngramList
					end;
				score := -.(log (wordTypesFloat /. (wordTypesFloat +. totalWordsFloat)));
				score := !score +. (prob_phonemes word wordNgramCountsArray wordTotalNgramsArray wordTypesWithCountArray (max) (+.));
				(* score := !score +. (prob_phonemes word ngramCountsArray totalNgramsArray wordTypesWithCountArray (max) (+.)); *)
		end;
	(* printf "\nScore for %s = %e\n" word !score; *)
	!score;;

let rec mbdp_inner subUtterance firstChar lastChar bestList =
	if firstChar <= lastChar then
		begin
			let newSubUtterance = String.sub subUtterance firstChar ((lastChar + 1) - firstChar) in
			let wordScore = evalWord newSubUtterance in
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

let evalUtterance sentence =
	let lastCharList = Array.init (String.length sentence) (fun a -> a) in
	let bestList = Array.fold_left
		(fun oldBestList lastChar ->
			(* printf "LastChar: %i\tString length: %i\n" lastChar (String.length sentence); *)
			let subUtterance = String.sub sentence 0 (lastChar + 1) in
			let word = String.slice ~first:(lastChar + 1) sentence in
			let newBestList = Array.append oldBestList [|((evalWord subUtterance), 0)|] in
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
			let wordNgramList = List.init wordWindow (fun a -> a) in
			(* printf "startChar = %d\tendChar =%d\n" startChar endChar; *)
			printf "%s" (newWord ^ !wordDelimiter);
			totalWords := !totalWords + 1;
			if !featureFile <> "" then
				begin
					List.iter (* Get feature n-gram counts of all size *)
						(fun currentWindowSizeMinusOne ->											
							let firstCharListForBundles = Array.init (String.length wordWithBoundary) (fun a -> a) in
							let firstCharList = List.init ((String.length wordWithBoundary) - currentWindowSizeMinusOne) (fun a -> a) in
							let wordFeatures = Array.map (* Build an array of all feature bundles in current word *)
													(fun firstChar ->
														let phoneme = String.sub wordWithBoundary firstChar 1 in												
														features_for_phone phoneme
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
												Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) featureGram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) featureGram) + 1)
											else
												Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) featureGram 1;
											Array.set totalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) + 1)
										)
										ngramFeatureSet;							
								)
								firstCharList;
						)
						wordNgramList
				end
			else
				begin
					let wordNgramList = List.init wordWindow (fun a -> a) in
					List.iter (* Get n-gram counts of all size *)
						(fun currentWindowSizeMinusOne ->
							let ngramFirstCharListLength = (String.length wordWithBoundary) - currentWindowSizeMinusOne in 
							let ngramFirstCharList = List.init ngramFirstCharListLength (fun a -> a) in
							List.iter (* Loop through all n-grams of current size *)
								(fun firstChar ->
									let ngram = String.sub wordWithBoundary firstChar (currentWindowSizeMinusOne + 1) in
									if Hashtbl.mem ngramCountsArray.(currentWindowSizeMinusOne) ngram then
										Hashtbl.replace ngramCountsArray.(currentWindowSizeMinusOne) ngram ((Hashtbl.find ngramCountsArray.(currentWindowSizeMinusOne) ngram) + 1)
									else
										Hashtbl.add ngramCountsArray.(currentWindowSizeMinusOne) ngram 1;
								)
								ngramFirstCharList;
							Array.set totalNgramsArray currentWindowSizeMinusOne (totalNgramsArray.(currentWindowSizeMinusOne) + ngramFirstCharListLength)
						)
						wordNgramList
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
	begin
		(* The prompt below would not print out above the input, so I commented it out.*)
		sentenceList := Std.input_list stdin;
		close_in stdin
	end;;

(* Read feature file, if specifed *)
if !featureFile <> "" then
	read_feature_file !featureFile;;

(* Loop through sentences *)
List.iter
	(fun segmentedSentence -> 
		let sentence = replace ~rex:removeSpacesPattern ~templ:"" segmentedSentence in (* Removes spaces from sentence *)
		let bestStartList = evalUtterance sentence in
		let segmentation = (List.fast_sort compare (find_segmentation bestStartList (Array.length bestStartList) [])) @ [String.length sentence] in
		if (!displayLineNumbers) then
			printf "%d: " ((Hashtbl.find lexicon !utteranceDelimiter) + 1);
		lexicon_updater segmentation sentence; 
		if (!printUtteranceDelimiter) then
			printf "%s" !utteranceDelimiter;		
		printf "\n";
		flush stdout;
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
	List.iter
		(fun currentWindowSizeMinusOne ->
			fprintf oc "CURRENT WINDOW SIZE: %d\n" (currentWindowSizeMinusOne + 1); 
			hash_fprint oc ngramCountsArray.(currentWindowSizeMinusOne);			
		)
		ngramList;
	close_out oc;;
	
