(* OCaml port of featurechart.pm *)

open ExtList
open ExtString

let badLeft = "⟪"
let badRight = "⟫"
let phonesToFeatures = Hashtbl.create 100
let featuresToPhones = Hashtbl.create 100
let features = ref [];;

module StringSet = Set.Make(String);;

(* Read feature file *)
let read_file featureFile =
	let ic = open_in featureFile in
	try
		let fileLines = Std.input_list ic in
		close_in ic;
		(* Fill feature list *)
		List.iter 
			(fun feature ->	
				features := !features @ [feature])
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
																oldFeatureSet)
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
																	"")
															featureList)) in
				Hashtbl.add phonesToFeatures phone currentFeatureSet)
			(List.tl fileLines);
	with e ->
		close_in_noerr ic;
		raise e;;
		
let features_for_phone phone =
	Hashtbl.find phonesToFeatures phone;;
