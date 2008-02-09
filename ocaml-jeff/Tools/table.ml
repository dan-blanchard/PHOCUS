(* 

   This file contains functions for making and using tables
   implemented as a matrix (array of arrays)
   author: Jeff Heinz
   Last Updated: June 8, 2006
  
*)

type t = string array array

let of_stringlist ?col_delim:(col_delim='\t') sl =
  (* makes a table from a string list (representing rows) where
     the cols are delimited in each row by the col_delim *)
  let rec process_sl stringlist arrayList =
    match stringlist with
      | [] -> arrayList
      | h::t -> 
  	  let list = Mstring.break_at col_delim h in
 	  let arr = Array.of_list list in
	  process_sl t (arr::arrayList) 
  in
  let arraylist = process_sl sl [] in
  Array.of_list (List.rev arraylist)


let to_file ?col_delim:(col_delim="\t") t filename =
  let stringList = 
    List.map 
      (fun x -> Mstring.concat_list col_delim (Array.to_list x) "")
      (Array.to_list t) 
  in
  File.of_stringlist filename stringList


let of_file ?col_delim:(col_delim='\t') filename  = 
  let sl = File.to_stringlist filename in
  of_stringlist ~col_delim:col_delim sl


let get_row_names table = 
  (* the row_names are the first element in each row after the
     first row *)
  (List.tl (Array.to_list (Array.map (fun x -> x.(0)) table)))  

let get_col_names table =
  (* the col names are the elements in the first row excluding the
     first column *)
  (List.tl (Array.to_list table.(0)))  

let get_val_pos x y t = t.(x).(y)

let num_in_list list a =
  (* takes a list and an element and returns the position of the
     element in the list. Raises Not_found if not in the list *)
  let rec helper l n =
    match l with
      | [] -> raise Not_found
      | h::t -> if a = h then n else helper t (n+1)
  in
  helper list 0

let get_val table row col rowFailure colFailure=
  (* takes a table, a row name and a col name and two strings to
     indicate how a failure should be specified and returns the value
     of the table at that cell or returns some error message *)
  let colNameList = get_col_names table in
  let rowNameList = get_row_names table in
  let rf =
    if rowFailure = "" then "The row name given is undefined"
    else rowFailure
  in
  let cf =
    if colFailure = "" then "The col name given is undefined"
    else colFailure
  in
  let colIndex = 
    try num_in_list colNameList col 
    with Not_found -> failwith cf
  in
  let rowIndex = 
    try num_in_list rowNameList row 
    with Not_found -> failwith rf
  in
  (table.(rowIndex+1).(colIndex+1))
  


