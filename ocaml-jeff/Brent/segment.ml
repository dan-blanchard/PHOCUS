
let usage () = print_endline (
    "\nUSAGE: segment [options] [infile]\n\n"^
      "See the documentation file for more information.\n");;

let cmd_list = List.tl (Array.to_list Sys.argv);;

(* the cmd array keeps track of the options. 
   
   0 - contains the seg delimiter
   1 - contains the word delimiter
   2 - contains the lexfile (optional)
   3 - records whether diagnostic messages should be output
   4 - contains the infile (optional)
   5 - records whether there was an error
*)

let a_size = 6;;

let seg_pos = 0;;
let wrd_pos = 1;;
let lex_pos = 2;;
let msg_pos = 3;;
let fle_pos = 4;;
let err_pos = 5;;


let cmd_array = Array.make a_size "";;
Array.set cmd_array wrd_pos " ";;

let rec process_args list = 
  match list with
    | [] -> ()
    | "-sd"::(seg_delim::t) ->
	Array.set cmd_array seg_pos seg_delim; process_args t
	  
    | "-wd"::(wd_delim::t) ->
	Array.set cmd_array wrd_pos wd_delim; process_args t
	  
    | "-l"::(lexfile::t) ->
	Array.set cmd_array lex_pos lexfile; process_args t
	  
    | "-v"::t ->
	Array.set cmd_array msg_pos "true"; process_args t

    | file::[] -> Array.set cmd_array fle_pos file 

    | _ -> Array.set cmd_array err_pos "true" ;;


process_args cmd_list;;

let seg_delim = Array.get cmd_array seg_pos;;
let wd_delim = Array.get cmd_array wrd_pos;;

module Seg_D = struct let lb = "" let rb = "" let delim = seg_delim end;;
module Word_D = struct let lb = "" let rb = "" let delim = wd_delim end;;
module B = Brent.Make(S)(Seg_D)(Word_D);;

let main () =
  if Array.get cmd_array err_pos = "true" 
  then let () = usage () in 
    failwith "Unrecognized arguments. See documentation."
  else 
    let lexicon = 
      let lexfile = Array.get cmd_array lex_pos in
      if lexfile = "" then B.empty_lexicon
      else B.lexicon_of_file lexfile
    in
    let msg = if Array.get cmd_array msg_pos = "true"
      then true 
      else false
    in
    let infile = Array.get cmd_array fle_pos in
    B.segmenter ~msg:msg ~infile:infile lexicon;;

main ();;
