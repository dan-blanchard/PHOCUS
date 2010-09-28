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
open Set

type feature_chart = 
{ 
	phonesToFeatures : (string, StringSet.t) Hashtbl.t;
	featuresToPhones : (string, StringSet.t) Hashtbl.t;
	mutable features : string list
}

(* Reads in the feature chart from a file *)
val read_feature_file : string -> unit

(* Returns a StringSet of features given a phone*)
val features_for_phone : string -> StringSet.t

(* Returns a StringSet of phones given a StringSet of features*)
val phones_for_features : StringSet.t -> StringSet.t

(* Prints a StringSet of features *)
val print_string_set : StringSet.t -> unit
