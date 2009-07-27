(*	
	PHOCUS: PHOnotactic CUe Segmenter - Feature Chart Module
	Copyright (C) 2007-2009 Dan Blanchard.
	
	This file is part of PHOCUS.
	
	PHOCUS is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
    
	Foobar is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
    
	You should have received a copy of the GNU General Public License
	along with PHOCUS.  If not, see <http://www.gnu.org/licenses/>.
*)

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

(* Returns a StringSet of phones given a StringSet of features*)
val phones_for_features : Set.Make(String).t -> Set.Make(String).t

(* Prints a StringSet of features *)
val print_string_set : Set.Make(String).t -> unit
