(* OCaml port of featurechart.pm *)

open ExtList
open ExtString
open Printf

module StringSet =  Set.Make(String)

type feature_chart = 
{ 
	phonesToFeatures : (string, Set.Make(String).t) Hashtbl.t;
	featuresToPhones : (string, Set.Make(String).t) Hashtbl.t;
	mutable features : string list
}

let chart = 
{
	phonesToFeatures = Hashtbl.create 100;
	featuresToPhones = Hashtbl.create 100;
	features = []
}

let read_feature_file featureFile =
	let ic = open_in featureFile in
	try
		let fileLines = Std.input_list ic in
		close_in ic;
		(* Fill feature list *)
		List.iter 
			(fun (feature:string) ->	
				chart.features <- chart.features @ [feature]
			)
			(String.nsplit (String.lchop (List.hd fileLines)) "\t");
		(* Fill chart.phonesToFeatures and chart.featuresToPhones tables*)
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
																	let featureValue = value ^ (List.nth chart.features index) in
																	if Hashtbl.mem chart.featuresToPhones featureValue then
																		Hashtbl.replace chart.featuresToPhones featureValue (StringSet.add phone (Hashtbl.find chart.featuresToPhones featureValue))
																	else
																		Hashtbl.add chart.featuresToPhones featureValue (StringSet.add phone StringSet.empty);
																	featureValue
																else
																	""
															)
															featureList)) in
				Hashtbl.add chart.phonesToFeatures phone currentFeatureSet
			)
			(List.tl fileLines);
	with e ->
		close_in_noerr ic;
		raise e;;

let features_for_phone phone =
	Hashtbl.find chart.phonesToFeatures phone;;

let print_string_set stringSet = 
	StringSet.iter
		(fun currentFeature -> 
			printf "%s\t" currentFeature
		)
		stringSet;
	printf "\n";;




