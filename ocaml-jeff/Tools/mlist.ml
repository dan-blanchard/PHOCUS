(*

  The Mlist module contains more functions for manipulating lists.

   Author: Jeff Heinz.

   Last Updated: May 10, 2006.
  
*)


let cross xlist1 xlist2 = 
  let foldFunction1 a x1 =
    let foldFunction2 a2 x2= 
      (x1,x2)::a2
    in
    List.fold_left foldFunction2 a xlist2
  in 
  List.fold_left foldFunction1 [] xlist1

let get_index list a =
  let rec helper l n =
    match l with
      | [] -> raise Not_found
      | h::t -> if a = h then n else helper t (n+1)
  in
  helper list 0

let to_string list elt_to_string delim lstring rstring empty_string = 
  if list = [] then empty_string
  else
    let sl = List.map elt_to_string list in
    lstring^(Mstring.concat_list delim sl "")^rstring
      

let of_string string elt_of_string delim lb rb empty_string =
  let meat,_ = Mstring.extractBetweenDelimiters string lb rb 0
  in
  if meat = empty_string then []
  else if meat = "" then 
    failwith "Empty string alert: Check your delimiters (or use <lambda> to indicate the empty list)"
  else
    let stringList = Mstring.to_stringlist delim meat in
    List.map elt_of_string stringList

let is_subset list1 list2 = 
  let rec helper l =
    match l with 
      | [] -> true
      | h::t -> 
	  if List.mem h list2 then helper t
	  else false
  in 
  helper list1

let rec remove elt list =
  match list with
    | [] -> []
    | h::t -> 
	if h = elt 
	then remove elt t
	else 
	  h::(remove elt t)

	    
let suffixes list =
  let rec tails l =
    match l with 
      | [] -> []
      | h::t -> t::(tails t)
  in
  list::(tails list)
    
let prefixes list = List.map List.rev (suffixes (List.rev list))
    
