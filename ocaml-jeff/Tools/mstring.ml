(* This file contains functions for manipulating strings, characters and lists
   of those strings characters etc. It depends on the Str library included
   with OCaML.  
   
   author: Jeff Heinz 
   Last updated: July 18, 2006

*)

let tokenize string =
  (* converts a string to a char list *)
  let len = String.length string in
  let rec helper n list =
    if n = -1 then list
    else 
      helper (n-1) (string.[n]::list)
  in
  helper (len-1) []

let break_at c s =
  let len = String.length s in
  let rec helper curPos lastBreak outList =
    if curPos < 0 then ((String.sub s 0 (lastBreak+1))::outList)
    else if c = s.[curPos] then helper (curPos-1) (curPos-1)
      ((String.sub s (curPos+1) (lastBreak - curPos))::outList)
    else helper (curPos-1) lastBreak outList
  in 
  helper (len - 1) (len - 1) []


let count_char c s = 
  let sl = break_at c s in
  (List.length sl)-1


let whitespace = "[ \t\n]*"

let to_stringlist delim string = 
  let r = Str.regexp (whitespace^(Str.quote delim)^whitespace) in
  Str.split r string

(*
let to_stringlist delim string = 
  if delim = "" then
    List.map Char.escaped (tokenize string)
  else
    break_at delim.[0] string
*)

let rec print_list stringlist = 
  match stringlist with
    | [] -> ()
    | h::t -> 
	let () = print_string (h^"\n") in
	print_list t


let split_at n s =
  if n >= (String.length s) 
  then failwith "split_at"
  else
    let len = String.length s in
    let part1 = String.sub s 0 (n+1) in
    let part2 = String.sub s (n+1) (len - n - 1) in
    part1, part2


let rec concat_list delimiter sl s =
  match sl with
    | [] -> s
    | h::[] -> (h^(concat_list delimiter [] s))
    | h::t -> (h^delimiter^(concat_list delimiter t s))

let remove_char c s = 
  let s1 = break_at c s in
  concat_list "" s1 ""

let remove_chars cl s = 
  List.fold_left 
    (fun s c -> remove_char c s) s cl


let extractBetweenDelimiters s lbrace rbrace curPos =
  match lbrace,rbrace with
    | ("","") -> s,String.length s
    | ("",_) ->  
	let rr = Str.regexp (whitespace^(Str.quote rbrace)^whitespace) in
	let fc_of_rb = 
	  try let _ = Str.search_forward rr s 0 in Str.match_beginning ()
	  with Not_found -> raise (Failure("Right Delimiter "^rbrace^" is missing"))
	in
	Str.string_before s fc_of_rb, Str.match_end ()
    | (_,"") ->
	let lr = Str.regexp (whitespace^(Str.quote lbrace)^whitespace) in
	let lc_of_lb = 
	  try let _ = Str.search_forward lr s 0 in Str.match_end ()
	  with Not_found -> raise (Failure("Left Delimiter "^lbrace^" is missing"))
	in
	Str.string_after s lc_of_lb, String.length s
    | _ ->
	let lr = Str.regexp (whitespace^(Str.quote lbrace)^whitespace) in
	let rr = Str.regexp (whitespace^(Str.quote rbrace)^whitespace) in
	let lc_of_lb = 
	  try let _ = Str.search_forward lr s 0 in Str.match_end ()
	  with Not_found -> raise (Failure("Left Delimiter "^lbrace^" is missing"))
	in
	let string_after_lb = Str.string_after s lc_of_lb in
	let fc_of_rb = 
	  try let _ = Str.search_forward rr s lc_of_lb in
	      Str.match_beginning ()
	  with Not_found -> raise (Failure("Right Delimiter "^rbrace^" is missing"))
	in
	Str.string_before string_after_lb (fc_of_rb-lc_of_lb), Str.match_end ()


let white_chars = [' ';'\n';'\t']

let remove_whitespace s = remove_chars white_chars s 



(*
let extractBetweenDelimiters s lc rc curPos =
  try match lc,rc with
    | ("","") -> s,0
    | ("",_) ->  
	let rpos = String.index_from s curPos rc.[0] in
	(String.sub s curPos ((rpos-curPos)), rpos+1)
    | (_,"") ->
	let lpos = String.index_from s curPos lc.[0] in 
	(String.sub s (lpos+1) ((String.length s)-lpos-1),(String.length s))
    | _ ->
	let lpos = String.index_from s curPos lc.[0] in  
	let rpos = String.index_from s (lpos+1) rc.[0] in
	(String.sub s (lpos+1) (rpos-lpos-1),rpos+1)
  with _ -> failwith "Mstring.extractBetweenDelimiters: check your braces?"
*)


let to_pair c s =
  let pos = 
    try String.index s c
    with Not_found -> failwith "Mstring.to_pair: char Not Found"
  in 
  let part1,part2 = split_at pos s
  in 
  String.sub part1 0 ((String.length part1)-1),part2

