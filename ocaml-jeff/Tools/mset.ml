(*

  The Mset module contains more functions for manipulating sets.

   Author: Jeff Heinz.

   Last Updated: May 10, 2006.
  
*)

exception Formatting_Error

let cross add pair fold empty xSet1 xSet2 = 
  fold 
    (fun x1 a -> 
      fold 
	(fun x2 a2 ->
	  try (add (pair x1 x2) a2) 
	  with (Failure("NonCombinable")) -> a2 
	) xSet2 a
    ) xSet1 empty
    
let of_list list set_add set_empty =
  List.fold_left 
    (fun set x -> set_add x set)
    set_empty
    list

let empty_string = "<emptyset>"

let to_string set set_elements elt_to_string delim lbrace rbrace =
  Mlist.to_string (set_elements set) elt_to_string delim lbrace rbrace empty_string

let of_string string set_add set_empty elt_of_string delim lbrace rbrace =
  if string = empty_string
  then set_empty
  else
    let meat,_ = Mstring.extractBetweenDelimiters string lbrace rbrace 0
    in
  if lbrace <> "" && rbrace <> "" && meat = "" then set_empty
  else if meat = "" then     
    failwith "Empty string alert: Check your delimiters (or use <emptyset> to indicate the empty set)"
  else
    let list = List.fold_left (* skip elements that are formed by the empty string *)
      (fun l x -> if x = "" then l else (elt_of_string x)::l)
      []
      (Mstring.to_stringlist delim meat)
    in
    of_list list set_add set_empty
      
let to_file filename set set_elements elt_to_string =
  (* this function writes each elt is to its own line. *)
  let s = Mlist.to_string (set_elements set) elt_to_string "\n" "" "" empty_string
  in 
  File.of_string filename s 
    
let of_file filename set_add set_empty elt_of_string =
  (* this function assumes each elt is on its own line. *)
  let sl = File.to_stringlist filename in
  List.fold_left (fun set x -> set_add (elt_of_string x) set)
    set_empty
    sl
    

let symdiff set1 set2 set_inter set_diff set_cardinal set_to_string eltname filename =
  let both = set_inter set1 set2
  in 
  let s1not2 = set_diff set1 set2
  in 
  let s2not1 = set_diff set2 set1
  in 
  let numBoth = string_of_int (set_cardinal both) in
  let num1not2 = string_of_int (set_cardinal s1not2) in
  let num2not1 = string_of_int (set_cardinal s2not1) in
  let headline = 
    "There are "^numBoth^" "^eltname^"s in both "^eltname^" sets.\n"^
    "There are "^num1not2^" "^eltname^"s in 1 but not in 2.\n"^
    "There are "^num2not1^" "^eltname^"s in 2 but not in 1.\n\n"
  in 
  let () = print_string headline
  in 
  let s = 
    (File.comment_string (headline^
    (String.capitalize eltname)^"s in both 1 and 2.\n"))^
    "\n"^(set_to_string both)^"\n\n"^
    (File.comment_string
      ((String.capitalize eltname)^"s in 1 but not 2.\n"))^
    "\n"^(set_to_string s1not2)^"\n\n"^
    (File.comment_string
      ((String.capitalize eltname)^"s in 2 but not 1.\n"))^
    "\n"^(set_to_string s2not1)^"\n\n"
  in 
  let () = File.of_string filename s in
  both,s1not2,s2not1


let powerset s set_cardinal set_elements set_add set_empty set_of_set_add set_of_set_empty =
  let num = set_cardinal s in
  let initial_bool_vector = Mbool.tuplemaker num in
  let bool_vector_list = Mbool.get_args [initial_bool_vector] in
  let elt_list = set_elements s in
  List.fold_left 
    (fun set_of_set bool_vector ->
      let combined = List.combine elt_list bool_vector in
      let subset =
	  List.fold_left
	    (fun set (elt,bool) ->
	      if bool then set_add elt set else set)
	    set_empty
	    combined
      in
      set_of_set_add subset set_of_set
    )
    set_of_set_empty
    bool_vector_list

(*
let are_disjoint set1 set2 set_inter set_is_empty =
  set_is_empty (set_inter set1 set2)

let is_pairwise_disjoint set_of_sets setset_fold set_equal set_inter set_is_empty =
  setset_fold (fun set1 bool1 ->
    setset_fold (fun set2 bool2 ->
      if set_equal set1 set2 then bool2
      else are_disjoint set1 set2 set_inter set_is_empty && bool2
    ) set_of_sets bool1
  ) set_of_sets true

(* e.g. set_of_sets is { {a,b}, {c}, {d,e} } and 
   the whole_set is {a,b,c,d,e} *)

let is_union set_of_sets whole_set setset_fold set_union set_empty set_equal =
  let union = setset_fold (fun set uset -> set_union set uset)
    set_of_sets set_empty
  in
  set_equal union whole_set

let is_partition set_of_sets whole_set setset_fold set_union set_inter 
    set_equal set_is_empty set_empty =
  is_union set_of_sets whole_set setset_fold set_union set_empty set_equal
  && is_pairwise_disjoint set_of_sets setset_fold set_equal set_inter set_is_empty
*)
    


(*
  let swap_elt a b ercset =        
(* replaces elt b with elt a in Set *)
    remove b (add a ercset)
*)
