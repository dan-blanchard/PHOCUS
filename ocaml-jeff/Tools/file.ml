(* 

   This file contains functions for reading and writing to files
   author: Jeff Heinz
   Last Updated: May 8, 2006

   NOTES: Empty lines and those whose first charcter is '%' are
   ignored. Thus comments may be added to files by using the '%'
   character at the beginning of the line.
*)


let comment_char = '%' 
    
let to_stringlist filename =
  (* converts a file to a string list *)
  let infile = open_in filename in
  let rec list_of_lines ifile =
    try 
      let line = input_line ifile in
      if  (line = "") || (line.[0] = '%') 
      then (list_of_lines ifile)
      else  
	line::(list_of_lines ifile)
    with 
      | End_of_file -> []
  in
  let sl = list_of_lines infile in
  let () = close_in infile in 
  sl 

let to_strict_stringlist filename =
  (* like to_stringlist but doesn't ignore lines *)
  let infile = open_in filename in
  let rec list_of_lines ifile =
    try 
      let line = input_line ifile in
      line::(list_of_lines ifile)
    with 
      | End_of_file -> []
  in
  let sl = list_of_lines infile in
  let () = close_in infile in 
  sl 

let to_string ?delim:(delim="") filename =
  (* converts a file to a string with *NO* delimiter 
     between lines as the default *)
  let sl = to_stringlist filename in
  Mstring.concat_list delim sl ""

let of_string filename string = 
  (* makes a file out of a string *)
  let outfile = open_out filename in
  let () = output_string outfile string in
  close_out outfile
    
let of_stringlist filename stringlist = 
  (* makes a file out of a string list. Each string is on its own line. *)
  let outfile = open_out filename in
  let rec write_stringlist list =
    match list with
      | [] -> close_out outfile
      | h::t -> 
	  let () = output_string outfile (h^"\n") in
	  write_stringlist t
  in
  write_stringlist stringlist

let append_to_front string filename = 
  (* appends a string to the front of a file *)
  let sl = to_strict_stringlist filename in
  of_stringlist filename (string::sl)

let comment_string string = 
  (Char.escaped comment_char)^" "^string

let name_of_string string =
  (* removes offending characters from string to make a legal
     filename *)
  let illegal_chars  =['.';'?';':';',';'{';'}';'[';']']
  in 
  List.fold_left 
    (fun s x -> Mstring.remove_char x s) 
    string 
    illegal_chars

let name_rm_ext string = 
  (* removes anything after the first '.' in the string *)
  try fst (Mstring.to_pair '.' string)
  with Not_found -> string

let name_get_ext string = 
  (* returns the portion of the string after the last '.' in the
     string *)
  try 
    let pos = String.rindex string '.' in
    snd (Mstring.split_at pos string)
  with Not_found -> string


let to_ignore filename = 
  (* returns true if filename begins with '.' or '#' or ends with
     '~' or is named "not_in_use" (which should be a directory) *)
  let l = String.length filename in
  let first_char = filename.[0] in
  let last_char = filename.[l-1] in
  first_char = '.'  
    || first_char = '#' 
    || last_char = '~' 
    || filename = "not_in_use"
    || filename = "Makefile"



let show n = 
  if n mod 100 == 0 then
    print_endline (string_of_int n)
  else ()


let iter filename f =
  (* applies function f: string -> unit to every line in the
     filename *)
  let infile = open_in filename in
  let eof = ref false in
  while 
    not !eof
  do
    try
      let line = input_line infile in
      if  (line = "") || (line.[0] = comment_char) 
      then ()
      else f line
    with 
      | End_of_file -> eof := true
  done;
  close_in infile
    
    
let dir_apply funct dirname =
  (* applies the funct: string -> unit to every file in the
     directory dirname, excludng those which should be ignored by
     to_ignore *)
  let dir = Unix.opendir dirname 
  in
  let rec helper d = 
    try 
      let file = Unix.readdir d in
      let () = 
	if to_ignore file 
	then () 
	else funct file
      in 
      helper d
    with
      | End_of_file -> () 
  in   
  let () = helper dir
  in 
  Unix.closedir dir

let dir_make string = 
  (* makes a directory accessible to all *)
  Unix.mkdir string 493

let dot2x filename x =
  (* turns a dot file into a x file *)
  let name = name_rm_ext filename in
  Unix.system ("dot -T"^x^" "^filename^" -o "^name^"."^x)
	
	
let read () = 
  let eof = ref false in
  let sl = ref [] in
  while not !eof 
  do 
    try
      let s = read_line () in
      if s = "<eof>" then eof := true
      else sl := s::!sl;
    with End_of_file -> eof := true 
  done;
  Mstring.concat_list "\n" (List.rev !sl) ""

(*   let rec helper () =  *)
(*     try *)
(*       let s = read_line () in *)
(*       if s = "<eof>" then [] *)
(*       else s::(helper ()) *)
(*       with End_of_file -> [] *)
(*   in *)
(*   Mstring.concat_list "\n" (helper ()) "";; *)



let fold f infile a =
  (* f must be a function of type : string -> a -> a *)
  let eof = ref false in
  let aref = ref a in
  while 
    not !eof
  do try
      let line = input_line infile in
      if  (line = "") || (line.[0] = '%') 
      then ()
      else aref := f line !aref
    with 
      | End_of_file -> eof := true
  done;
  close_in infile;
  !aref

(*   try let s = input_line in_channel in *)
(*       if s = "" || s.[0]=comment_char *)
(*       then fold f in_channel a *)
(*       else fold f in_channel (f s a) *)
(*   with End_of_file -> a *)
