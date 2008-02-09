(** 

  The Mlist module contains more functions for manipulating lists.

   Author: Jeff Heinz.

   Last Updated: May 10, 2006.
  
*)


(** 
  [cross l1 l2] returns a list of pairs. The first element in
  the pair is an element of [l1] and the second element is of
  [l2].
*)
val cross : 'a list -> 'b list -> ('a * 'b) list 


(** 
  [get_index l a] returns the integer position of [a] in [l]. The head
  of [l] is 0. Raises [Not_found] if [a] is not in [l].
*)
val get_index : 'a list -> 'a -> int 

(** [to_string list elt_to_string delim lstring rstring empty_string] takes
    [list] and returns a string where [elt_to_string] is a function of elements
    of [list] to strings, [delim] is a string delimiter between elements.
    [lstring] and [rstring] are concatenated to the left and right sides,
    repsectively. If the list is empty, then [empty_string] is returned.
*)
val to_string : 'a list -> ('a -> string) -> string -> string -> 
  string -> string -> string

(**
   [of_string string elt_of_string delim lbrace rbrace] takes [string] and returns a
   list of elements, where the elements are the result of applying to
   [elt_of_string] to substrings of [string] delimited by [delim].  
   The empty list is returned iff the [string = empty_string].
*)
val of_string : string -> (string -> 'a) -> string -> string -> 
  string -> string -> 'a list

(** 
  [remove a l] removes all occurences of [a] in [l]. If there are no
  occurences of [a] in [l], [l] is returned.
*)
val remove : 'a -> 'a list -> 'a list 

(** 
  [is_subset l1 l2] returns [true] iff every element in [l1] is an
  element in [l2].
*)
val is_subset : 'a list -> 'a list -> bool 


(** Returns the prefixes of the list as a list. *)
val prefixes : 'a list -> 'a list list

(** Returns the suffixes, or tails, of the list as a list. *)
val suffixes : 'a list -> 'a list list
