(*
  This module implements some extra functions over maps.

  author: Jeff Heinz

  last updated: August 4, 2006

*)

module type XMAP_TYPE =
  sig
    include X.X_TYPE
    type key
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
    val to_file: string -> t -> unit
    val of_file: string -> t
    val size: t -> int
  end


module Make 
  (Key: X.X_TYPE)
  (Data: X.X_TYPE)
  (D: Delim.DELIM_TYPE) =

struct
  module Xmap = Map.Make(Key)
  type t = Data.t Xmap.t

  type key = Key.t
  type data = Data.t

  (* the delimiter indicating the relation, typically "\t" *)
  let relation_delim  = D.delim
  (* the delimiter seperating elements of the map, typically "\n" *)
  let elt_delim = D.rb 

  let name = ("Map ("^Key.name^"->"^Data.name^")")

  let compare = Xmap.compare Data.compare 

  let smaller d1 d2 = 
    if Data.compare d1 d2 >= 0 
    then d2
    else d1

  (* the pairing of two maps keeps only the least value 
     for the identical keys  *)
  let pair m1 m2 = 
    Xmap.fold 
      (fun k d1 m -> 
	try 
	  let d2 = (Xmap.find k m2) in
	  Xmap.add k (smaller d1 d2) m
	with Not_found -> Xmap.add k d1 m
      )
      m1
      Xmap.empty

  let of_string s = Mmap.of_string  
    s elt_delim relation_delim Xmap.add Xmap.empty Key.of_string Data.of_string
      
  let to_string m =  
    Mmap.to_string relation_delim elt_delim m Xmap.fold Key.to_string Data.to_string 
      
  let print ?oc:(oc=stdout) m = 
    Mmap.print ~oc:oc m Xmap.iter relation_delim elt_delim Key.to_string Data.to_string

  let print_ = print

  let empty = Xmap.empty
  let is_empty = Xmap.is_empty
  let add = Xmap.add
  let find = Xmap.find
  let remove = Xmap.remove
  let mem = Xmap.mem
  let iter = Xmap.iter
  let fold = Xmap.fold

  let equal = Xmap.equal 
    (fun x y -> if Data.compare x y = 0 then true else false)

  let to_file filename map = 
    Mmap.to_file filename relation_delim elt_delim map fold Key.to_string Data.to_string

  let of_file filename = 
    Mmap.of_file filename
      relation_delim elt_delim add empty Key.of_string Data.of_string

  let size m = Mmap.size m fold

end
