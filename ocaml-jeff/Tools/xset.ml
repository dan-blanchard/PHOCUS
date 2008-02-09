(* 
   This module basically extends the Set.Make module.  The analog of
   Set.OrderedType is [X.OrdType] whose type is [X.t].  [X.OrdType] requires a
   name of the item (such as `phone' for phonemes) and a few functions
   describing how to convert [X.t] to strings and back again.  [X.Make] returns
   the functions from [Set.Make] as well as some useful functions converting
   strings to sets and vice versa.

   author: Jeff Heinz

   last updated: July 8, 2006

*)


module type XSET_TYPE =
  sig
    include X.X_TYPE
    type elt
    val of_list : elt list -> t
    val to_file : string -> t -> unit
    val of_file : string -> t
    val cross : t -> t -> t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val max_elt: t -> elt
    val choose: t -> elt
    val split: elt -> t -> t * bool * t
  end



module Make
  (D : Delim.DELIM_TYPE)
  (X : X.X_TYPE) =
  
struct
  module Xset = Set.Make(X)    
  include Xset 

  let name = ("Set("^(X.name)^")")  

  let to_string set =
    Mset.to_string set elements X.to_string D.delim D.lb D.rb

  let of_string s = 
    try
      Mset.of_string s add empty X.of_string D.delim D.lb D.rb
    with | (Failure(x)) -> raise (Failure(x))
      |_ -> failwith (name^".of_string: Formatting Error. (check your braces?)")

  let print ?oc:(oc=stdout) set = 
    output_string oc (to_string set); output_string oc "\n";
    flush oc

  let print_ ?oc:(oc=stdout) set = 
    output_string oc D.lb; 
    iter (fun x -> X.print_ ~oc x; output_string oc D.delim) set; 
    output_string oc D.rb;
    flush oc

  let of_list list =  Mset.of_list list add empty

  let of_file filename = 
    let sl = File.to_stringlist filename in
    List.fold_left
      (fun set s ->
	add (X.of_string s) set)
      empty
      sl
      
  let to_file filename set = 
    let sl = List.map X.to_string (elements set) in
    File.of_stringlist filename sl

  let cross set1 set2 = Mset.cross add X.pair fold empty set1 set2

  let pair = union


end
