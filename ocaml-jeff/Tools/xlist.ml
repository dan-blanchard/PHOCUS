module type XLIST_TYPE = 
sig
  include X.X_TYPE
  type elt
  val build : elt -> t -> t
  val empty : t
  val empty_string : string
  val suffixes : t -> t list
  val prefixes : t -> t list
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


module Make
  (D : Delim.DELIM_TYPE)
  (X : X.X_TYPE) =
struct
  type elt = X.t
  type t = X.t list
  let name = ("List("^(X.name)^")")
  let compare = compare

  (* eventually can pass in all the list functions. *)

  let empty = []
  let length = List.length
  let hd = List.hd
  let tl = List.tl
  let rev = List.rev
  let append = List.append
  let nth = List.nth
  let mem = List.mem
  let iter = List.iter 
  let map = List.map
  let rev_map = List.rev_map
  let fold_left = List.fold_left
  let fold_right = List.fold_right

  (* I want to change this so that w1 and w2 do not have
     to be the same length.  *)

  let build elt list = elt::list

  let pair w1 w2 = 
    if (List.length w1) <> (List.length w2)
    then raise (Failure("NonCombinable"))
    else List.map2 X.pair w1 w2
    
  let prefixes = Mlist.prefixes
  let suffixes = Mlist.suffixes
    
  let empty_string = "<lambda>"

  let to_string w = Mlist.to_string w X.to_string D.delim D.lb D.rb empty_string 
  let of_string s = Mlist.of_string s X.of_string D.delim D.lb D.rb empty_string 

  let print ?oc:(oc=stdout) list = 
    output_string oc (to_string list);
    output_string oc "\n";
    flush oc

  let print_ ?oc:(oc=stdout) list = 
    output_string oc D.lb; 

    iter (fun x -> X.print_ ~oc x; output_string oc D.delim) list; 
    output_string oc D.rb;
    flush oc

end



(*
  let space num alphabet = 
  let rec helper n currentset totalset =
  match n with 
  | 0 -> totalset
  | _ -> 
  let nextset = 
  set_fold
  (fun w s ->
  X.set_fold
  (fun a s2 ->
  set_add (build a w) s2
  )
  alphabet
  s
  )
  currentset
  set_empty
  in 
  helper (n-1) nextset (set_union nextset totalset)
  in
  helper num (set_singleton empty) (set_singleton empty)
*)
