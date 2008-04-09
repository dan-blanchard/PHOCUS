(* OCaml port of featurechart.pm *)

let badLeft = "⟪"
let badRight = "⟫"
let phonesToFeatures = Hashtbl.create 100
let featuresToPhones = Hashtbl.create 100
let features = ref [];;

module FeatureSet = Set.Make(String);;

(* Read feature file *)
let read_file featureFile =
	let ic = open_in featureFile in
	try
		let fileLines = Std.input_list ic in
		close_in ic;
		List.iter 
			(fun feature ->	
				features := !features @ [feature])
			String.split (String.lchop (List.hd fileLines)) "\t";
		List.iter
			(fun line ->
				let lineList = String.split line "\t" in
				let phone = lineList.hd in
				let featureList = lineList.tl in
				let currentFeatureSet = FeatureSet.empty in 
				(* Create new feature set below containing all features in feature values in featureList appended with the corresponding feature names *)
				List.iteri 
					(fun index value ->
						
						if Hashtbl.mem phonemeCounts phoneme then
							Hashtbl.replace phonemeCounts phoneme ((Hashtbl.find phonemeCounts phoneme) + 1)
						else
							Hashtbl.add phonemeCounts phoneme 1								
					)
					featureList)
			List.tl fileLines
	with e ->
		close_in_noerr ic;
		raise e;;
		