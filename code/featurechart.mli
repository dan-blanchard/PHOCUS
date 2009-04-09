(* OCaml port of featurechart.pm *)

open ExtString

type feature_chart = 
{ 
	phonesToFeatures : (string, Set.Make(String).t) Hashtbl.t;
	featuresToPhones : (string, Set.Make(String).t) Hashtbl.t;
	mutable features : string list
}

(* Reads in the feature chart from a file *)
val read_feature_file : string -> unit

(* Returns a StringSet of features given a phone*)
val features_for_phone : string -> Set.Make(String).t

(* Prints a StringSet of features *)
val print_string_set : Set.Make(String).t -> unit
