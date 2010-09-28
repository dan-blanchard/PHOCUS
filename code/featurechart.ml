(*	
	PHOCUS: PHOnotactic CUe Segmenter - Feature Chart Module
	Copyright (C) 2007-2010 Dan Blanchard.
	
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

open Batteries
open List
open String
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
		let fileLines = List.of_enum (BatIO.lines_of ic) in
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
	
let phones_for_features featureSet = 
	if (StringSet.is_empty featureSet) then
		Hashtbl.fold 
			(fun key value currentSet ->
				StringSet.add key currentSet
			)
			chart.phonesToFeatures
			StringSet.empty
	else
		let first = ref true in 
		let resultSet = StringSet.fold
							(fun featureValue currentSet ->
								if (!first) then
									begin
										first := false;
										StringSet.union currentSet (Hashtbl.find chart.featuresToPhones featureValue)
									end
								else
									StringSet.inter currentSet (Hashtbl.find chart.featuresToPhones featureValue)
							)
							featureSet
							StringSet.empty
		in
		if (StringSet.is_empty resultSet) then
			failwith "No phones in chart match specified feature set!"
		else
			resultSet;;

let print_string_set stringSet = 
	StringSet.iter
		(fun currentFeature -> 
			printf "%s\t" currentFeature
		)
		stringSet;
	printf "\n";;




