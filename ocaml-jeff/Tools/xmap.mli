(**
  Xmap implements some extra functions over maps.

  author: Jeff Heinz

  last updated: August 4, 2006

*)


(** The output signature of [Make]. *)
module type XMAP_TYPE =
  sig

(** {2 X_TYPE functions}*)

    include X.X_TYPE
    (** The following list summarizes the functions in {!X.X_TYPE}.
	- Type [t] is the map.
	- [name] is ["Map ("^(Key.name)^" -> "^Data.name^")"]. 
	- [compare m1 m2] is the same as [Map.Make.compare Data.compare m1 m2]. 
	- [pair] is an intersection of maps.
	- [of_string] returns a map from its string representation. 
	- [to_string] returns a string representation of a map. 
	- [print] prints a map followed by an endline.
	- [print_] prints a map but no endline.
    *)

(** {2 XMAP_TYPE functions}*)

    (** The following functions are the corresponding functions in [Map.Make] in
    the standard library. *)

    (** The type of key in the map. (I.e. the domain of the function.) *)
    type key

    (** The type of data in the map. (I.e. the range of the function.) *)
    type data

    val empty: t
    val is_empty: t -> bool
    val add: key -> data -> t -> t
    val find: key -> t -> data
    val remove: key -> t -> t
    val mem:  key -> t -> bool
    val iter: (key -> data -> unit) -> t -> unit
    val fold: (key -> data -> 'a -> 'a) -> t -> 'a -> 'a 
    val equal: t -> t -> bool

(** {2 Extra functions} *)

    (** Writes the map to a file. *)
    val to_file: string -> t -> unit

    (** Returns a map that has been written in a file. *)
    val of_file: string -> t

    (** Returns the number of mappings in the map. *)
    val size: t -> int
  end



(** The functor [Make] takes three modules and outputs a module of XMAP_TYPE. *)
module Make 
  (Key: X.X_TYPE)          (* domain *)
  (Data: X.X_TYPE)         (* range  *)
  (D: Delim.DELIM_TYPE) :

  XMAP_TYPE with type key = Key.t 
	    and type data = Data.t 

