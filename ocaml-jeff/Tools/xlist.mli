(** 
    Xlist implements lists as abstract types. It offers a few more functions
    for lists. Not all functions from the standard library are implemented but
    it's easy to add them if needed.

    author : Jeff Heinz

    last updated : July 12, 2006

*)

module type XLIST_TYPE = 
sig
  type elt
  include X.X_TYPE

  (** This adds a list element to the head of a list. *)
  val build : elt -> t -> t

  (** The empty list. *)
  val empty : t

  (** How the empty list is referred to when reading and writing strings. It is
      ["<lambda>"]. *)
  val empty_string : string

  (** Returns the suffixes of the list. *)
  val suffixes : t -> t list

  (** Returns the prefixes of the list *)
  val prefixes : t -> t list

  (** The following functions are identical to the ones in the [List] module
      in the standard iibrary.  More of these functions will be added as
      needed. *)

  val length : t -> int
  val hd : t -> elt
  val tl : t -> t
  val nth : t -> int -> elt
  val mem : elt -> t -> bool
  val rev : t -> t
  val append : t -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> 'b) -> t -> 'b list
  val rev_map : (elt -> 'b) -> t -> 'b list
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val fold_right : (elt -> 'b -> 'b) -> t -> 'b -> 'b

end

(** Functor building an implementation of lists given a module of
    {!Delim.DELIM_TYPE} and a module of {!X.X_TYPE} *)
module Make
  (D : Delim.DELIM_TYPE)
  (X : X.X_TYPE) :
      XLIST_TYPE with type elt = X.t 
		 and type t = X.t list


