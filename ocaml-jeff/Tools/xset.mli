(** 
   Xset basically extends the {!Set.Make} module.  The analog of
   Set.OrderedType is [X.OrdType] whose type is [X.t].  [X.OrdType] requires a
   name of the item (such as "phone" for phonemes) and a few functions
   describing how to convert [X.t] to strings and back again.  [X.Make] returns
   the functions from [Set.Make] as well as some useful functions converting
   strings to sets and vice versa.

   author: Jeff Heinz

   last updated: July 8, 2006

*)


module type XSET_TYPE =
  sig
    include X.X_TYPE
    (** The following list summarizes the functions in {!X.X_TYPE}.
	- Type [t] is the set.
	- [name] is ["Set ("^(X.name)^")"]. 
	- [compare] is the same as [Set.Make.compare]. 
	- [pair] is the same as [union].
	- [of_string] returns a set from its string representation. 
	- [to_string] returns a string representation of a set. 
	- [print] prints a set followed by an endline.
	- [print_] prints a set but no endline.
    *)
    type elt

    (** [of_list elt_list] returns a set of elements from a list of
      elements. *)
    val of_list : elt list -> t

  (** [to_file filename set] writes [set] to [filename]. Each element is
      written on its own line. *)
    val to_file : string -> t -> unit

    (** [of_file filename] returns a set from what is written in
      [filename]. Each element of the set should be written on its own line. *)
    val of_file : string -> t

    (** Returns a set formed by pairing each element of the first set with each
	element of the second set.  If the pairing of two elements in the set
	raises [(Failure("NonCombinable"}))] they are not included in the
	resulting set. See Mset.cross. *)
    val cross : t -> t -> t


    (** Modules of XSET_TYPE just add the few functions above to the functions
	found in [Set.Make]. These functions are listed here but see
	[Set.Make] for documentation. *)

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
  (X : X.X_TYPE) :
  XSET_TYPE with type elt = X.t
