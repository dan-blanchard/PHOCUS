
(** 

  The Mset module contains more functions for manipulating sets.
  See Set.Make in the standard library for typical set functions.

  Authors: Jeff Heinz and Greg Kobele

   Last Updated: May 8, 2006.
  
*)

(** [Noncombinable] is an exception that can be returned by some
  pairing function *)

(** [cross set_add elt_pair set_fold set_empty Set1 Set2] crosses the elements
    of two (typewise same) sets according to some pairing function
    ([elt_pair]). [set_add], [set_fold], [set_empty] are typically the
    corresponding functions from [Set.Make]. If two elements cannot be combined
    by [elt_pair], this function should throw [Failure("NonCombinable")], and
    the pairing of those elements will not be included in the resulting
    (crossed) set.
*)
val cross :
  ('a -> 'b -> 'b) ->
  ('c -> 'c -> 'a) ->
  (('c -> 'b -> 'b) -> 'd -> 'b -> 'b) -> 'b -> 'd -> 'd -> 'b 


(** [to_string set set_elements to_string delim lbrace rbrace] turns [set]
    into a string where the elements of the set are surrounded by [lbrace] and
    [rbrace] and delimited by [delim]. [set_elements] should be the function
    from [Set.Make] which takes a set and returns a list. [to_string] is
    a function of elements to strings.
*)
val to_string : 'a -> ('a -> 'b list) -> ('b -> string) -> 
  string -> string -> string -> string


(** [of_string string set_add set_empty elt_of_string delim lbrace rbrace]
    takes [string] and returns a set. [set_add] and [set_empty] should be the
    functions from [Set.Make]. [elt_of_string] is a function which turns a
    string into an element of the set. [delim] is the delimiter in the string
    representation of the set, and [lbrace] and [rbrace] are characters
    representing the left and right braces of the set, respectively.  *NOTE*:
    This function ignores empty string representations of elements of a
    set. Thus if your string was ["\[a,b,c,,e\]"] this function would return a
    set of 4 elements [{elt("a"),elt("b"),elt("c"),elt("e")}].  It will not
    return an element of the empty string. The reason for this is to
    facilitate reading of files when the delimiter is the newline string
    ["\n"] (as with reading edgesets in the Machine module). If you need to
    refer to make element of the empty string I suggest substituting another
    symbol, e.g. write ["\[a,b,c,<LAMBDA>,e\]"] instead.
*)
val of_string :  string -> ('a -> 'b -> 'b) -> 'b -> (string -> 'a) -> 
  string -> string -> string -> 'b


(** Writes [set] to [filename]. Each elt is written to its own line.*)
val to_file :  string -> 'a -> ('a -> 'b list) -> ('b -> string) -> unit

(** Returns a set is written in [filename].  This function assumes each elt is
    on its own line. *)
val of_file : string ->  ('a -> 'b -> 'b) -> 'b -> (string -> 'a) -> 'b

(**
   [of_list list set_add set_empty] takes [list] and returns a set
   where [set_add] and [set_empty] are typically the corresponding
   functions from [Set.Make].
*)
val of_list : 'a list -> ('a -> 'b -> 'b) -> 'b -> 'b 


(**
  [symdiff set1 set2 set_inter set_diff set_cardinal set_to_string
  eltname filename]
  takes [set1] and [set2] and returns a triple: the elements in both
  [set1] and [set2], the elements in [set1] but not [set2], and the
  elements in [set2] but not [set1]. [set_inter], [set_diff], and
  [set_cardinal] are typically the corresponding
  functions from [Set.Make]. [set_to_string] takes sets and returns
  them as strings. [eltname] is a string naming the type of element in
  the set, e.g. "string" or "horse". [symdiff] also writes these three
  sets to file [filename].
*)
val symdiff : 'a ->  'a ->  ('a -> 'a -> 'b) ->  ('a -> 'a -> 'b) ->
  ('b -> int) -> ('b -> string) -> string -> string -> 'b * 'b * 'b


(** 
    [powerset s set_cardinal set_elements set_add set_empty set_of_set_add
    set_of_set_empty] returns the powerset of set [s]. 
*)
val powerset : 'a ->  ('a -> int) -> ('a -> 'b list) -> ('b -> 'c -> 'c) -> 
  'c  -> ('c -> 'd -> 'd) -> 'd -> 'd 

(*
val are_disjoint : 'a -> 'b -> ('a -> 'b -> 'c) -> ('c -> 'd) -> 'd

val is_pairwise_disjoint : 'a ->
  (('b -> bool -> bool) -> 'a -> bool -> bool) ->
  ('b -> 'b -> bool) -> ('b -> 'b -> 'c) -> ('c -> bool) -> bool

val is_union : 'a ->  'b ->
  (('c -> 'd -> 'e) -> 'a -> 'f -> 'g) ->
  ('c -> 'd -> 'e) -> 'f -> ('g -> 'b -> 'h) -> 'h 
*) 
