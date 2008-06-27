
let usage () = print_endline (
    "\nUSAGE: check [options] testfile goldfile blocksize \n\n"^
      "See the documentation file for more information.\n");;

let cmd_list = List.tl (Array.to_list Sys.argv);;

(* the cmd array keeps track of the options. 
   
   0 - contains the seg delimiter
   1 - contains the word delimiter
   2 - contains the utterance delimiter
   3 - contains the testfile
   4 - contains the goldfile
   5 - contains the blocksize over which to compute precision and recall
   6 - contains the value mod of which is printed out
   7 - records whether there was an error
*)

let a_size = 8;;

let seg_pos = 0;;
let wrd_pos = 1;;
let utt_pos = 2;;
let tst_pos = 3;;
let gld_pos = 4;;
let blk_pos = 5;;
let mod_pos = 6;;
let err_pos = 7;;


let cmd_array = Array.make a_size "";;
Array.set cmd_array wrd_pos " ";; (* default word delimiter is space *)
Array.set cmd_array utt_pos "$";; (* default utterance boundary is $ *)
Array.set cmd_array blk_pos "500";; (* default blocksize is 500 *)
Array.set cmd_array mod_pos "500";; (* default modvalue is 500 *)

let rec process_args list = 
  match list with
    | [] -> ()
    | "-sd"::(seg_delim::t) ->
	Array.set cmd_array seg_pos seg_delim; process_args t
	  
    | "-wd"::(wd_delim::t) ->
	Array.set cmd_array wrd_pos wd_delim; process_args t

    | "-ud"::(utt_delim::t) ->
	Array.set cmd_array utt_pos utt_delim; process_args t

    | "-b"::(blocksize::t) ->
	Array.set cmd_array blk_pos blocksize; process_args t

    | "-m"::(modvalue::t) ->
	Array.set cmd_array mod_pos modvalue; process_args t

    | testfile::(goldfile::[]) -> 
	let () = Array.set cmd_array tst_pos testfile in
	Array.set cmd_array gld_pos goldfile 

    | _ -> Array.set cmd_array err_pos "true" ;;

process_args cmd_list;;

let seg_delim = Array.get cmd_array seg_pos;; (*delimits segments within a word*)
let wd_delim = Array.get cmd_array wrd_pos;; (*delimits words from each other*)
let ub_delim = Array.get cmd_array utt_pos;; (*delimits utterances from each other*)
let modvalue = int_of_string (Array.get cmd_array mod_pos);; (* gets the mod value*)

module Seg_D = struct let lb = "" let rb = "" let delim = seg_delim end;;
module Word_D = struct let lb = "" let rb = "" let delim = wd_delim end;;
module Utt_D = struct let lb = "" let rb = "" let delim = ub_delim end;;
module B = Brent.Make(S)(Seg_D)(Word_D)(Utt_D);;

module Seg = S;;
(* the word boundary *)  
let wb = Seg.of_string (Word_D.delim);;
  
module SegSet = Set.Make(Seg);;
module Word = Xlist.Make(Seg_D)(Seg);;
    
  (* the utterance boundary *)
let ub = Word.of_string (Utt_D.delim);;
let ub_string = Seg_D.delim^Utt_D.delim;;
  
module Delim_word = struct 
  let delim = (Seg_D.delim)^(Word_D.delim)^(Seg_D.delim)
  let rb = (Word_D.rb)
  let lb = (Word_D.lb)
end;;
  
module Utterance = Xlist.Make(Delim_word)(Word);;

type symbol = Blank | Letter of Seg.t;;

let sym_to_string = function
    Letter(x) -> Seg.to_string x 
  | Blank -> "";;

exception Different_Utterances;;

let rec make_pair_list test_list gold_list list = 
  match (test_list,gold_list) with
    | ([],[]) -> list
(*    | ([],[]) -> (Letter(wb),Letter(wb))::list                 (* if we have finished the recursion *)*)
    | (_,[]) 
    | ([],_) -> raise Different_Utterances
    | _ -> 
	begin
	  let test_hd = List.hd test_list and
	      gold_hd = List.hd gold_list
	  in
	  if test_hd = gold_hd (* they match *)
	  then make_pair_list (List.tl test_list) (List.tl gold_list) ((Letter(test_hd),Letter(gold_hd))::list)

	  else if test_hd = wb (* test has wb but not gold *)
	  then make_pair_list (List.tl test_list) (gold_list) ((Letter(test_hd),Blank)::list)

	  else if gold_hd = wb (* gold has wb but not test *)
	  then make_pair_list (test_list) (List.tl gold_list) ((Blank,Letter(gold_hd))::list)

	  else raise Different_Utterances
	end;;


let print_pair_list pair_list = List.map (fun (a,b) -> print_endline (":"^(sym_to_string a)^":"^(sym_to_string b)^":")) (pair_list);;


let rec true_positives pair_list (wtp,btp) possible_word = 
(*  l6et _ = print_pair_list pair_list in
  let () = print_endline ((string_of_float tp)^":"^(string_of_bool possible_word)) in
*)
  match pair_list with
    |  [] -> wtp,btp
    | head::tail ->
	match head with
	  |  (Letter(x),Letter(y)) when (x=wb) && (y=wb) -> 
	       if possible_word 
	       then true_positives tail (wtp +. 1., btp +. 1.) true 
	       else true_positives tail (wtp, btp +.1.) true 
	  | (Blank,_) 
	  | (_,Blank) -> true_positives tail (wtp,btp) false
	  | _ -> true_positives tail (wtp,btp) possible_word (*maintain status quo*);;
  

let rec word_to_list w list = 
  if Word.empty = w then list
  else word_to_list (Word.tl w) ((Word.hd w)::list);;

let rec flatten_utterance u list =
  if u = Utterance.empty then list
  else
    let hd = Utterance.hd u and
	tl = Utterance.tl u
    in
    hd@(wb::(flatten_utterance tl list))


let rec compare_utterances test_u gold_u =
  let test_list = flatten_utterance test_u [] in
  let gold_list = flatten_utterance gold_u [] in
  (*
    let () = print_endline ("test as word:"
    ^(Mstring.concat_list Seg_D.delim (List.map Seg.to_string test_list) "")) 
    in
    let () = print_endline ("gold as word: "
    ^(Mstring.concat_list Seg_D.delim (List.map Seg.to_string gold_list) "")) 
    in
  *)
  let pair_list = make_pair_list test_list gold_list [] in
  (*
    let () = print_endline (Mstring.concat_list "\n" 
    (List.rev (List.map (fun (a,b) -> (sym_to_string a)^","^(sym_to_string b)) pair_list)) "")
    in
  *)
  let wtp,btp_temp = true_positives (List.rev pair_list) (0., 0.) true in   
    (* word true positives,  boundary true positives*)	 
  let btp = btp_temp -. 1. in
  let wfp = (float_of_int (Utterance.length test_u)) -. wtp in (* word false positives*) 
  let wfn = (float_of_int (Utterance.length gold_u)) -. wtp in (* word false negatives *)
  let bfp = (float_of_int (Utterance.length test_u) -. 1.) -. btp in(* boundary false positives*) 
  let bfn = (float_of_int (Utterance.length gold_u) -. 1.) -. btp in(* boundary false negatives *)
  (*  let () = print_endline ((string_of_float tp)^"\t"^(string_of_float fp)^"\t"^(string_of_float fn)) 
      in
  *)
  (wtp,wfp,wfn,btp,bfp,bfn);;

(* double_fold assumes the files are aligned line by line *)

let double_fold f file1 file2 a =
  (* f must be a function of type : string -> string -> a -> a *)
  let infile1 = open_in file1 and
      infile2 = open_in file2 and
      eof = ref false and
      aref = ref a 
  in
  while 
    not !eof
  do try
      let line1 = input_line infile1 in
      let line2 = input_line infile2 in
	aref := f line1 line2 !aref
    with 
      | End_of_file -> eof := true
  done;
    close_in infile1; close_in infile2;
    !aref

let precision tp fp = 
  tp /. (tp +. fp);;   (* true positives / (true positives + false positives ) *)

let recall tp fn = 
  tp /. (tp +. fn);;    (* true positives / (true positives + false negatives ) *)

let fscore p r = ( 2. *. p *. r ) /. (p +. r);; 


let update_scores blocksize test_string gold_string (wtp,wfp,wfn,btp,bfp,bfn,counter,array) =
  let test_u = Utterance.of_string test_string in
  let gold_u = Utterance.of_string gold_string in

  let uwtp,uwfp,uwfn,ubtp,ubfp,ubfn = 
    try compare_utterances test_u gold_u
    with Different_Utterances -> failwith ((string_of_int counter)^": Different Utterances!")
  in
  (*
    let () = print_endline ((string_of_int counter)^"\t test:"^(Utterance.to_string test_u)^
    "\t gold:"^(Utterance.to_string test_u)^"\t tp:"^(string_of_float utterance_tp)^
    "\t fp:"^(string_of_float utterance_fp)^"\t fn:"^(string_of_float utterance_fn))
    in
  *)
  if counter >= blocksize
  then 
    let position = counter mod blocksize 
    in
    let c_wtp,c_wfp,c_wfn,c_btp,c_bfp,c_bfn = Array.get array position
    in
    let  new_wtp,    new_wfp,    new_wfn,    new_btp,    new_bfp,    new_bfn = 
      wtp-.c_wtp, wfp-.c_wfp, wfn-.c_wfn, btp-.c_btp, bfp-.c_bfp, bfn-.c_bfn
    in
    let () = Array.set array position (uwtp,uwfp,uwfn,ubtp,ubfp,ubfn) 
    in
    let wprec = precision (new_wtp +. uwtp) (new_wfp +. uwfp)
    in
    let wrecl = recall (new_wtp +. uwtp) (new_wfn +. uwfn)
    in
    let wfscore = fscore wprec wrecl
    in 
    let bprec = precision (new_btp +. ubtp) (new_bfp +. ubfp)
    in
    let brecl = recall (new_btp +. ubtp) (new_bfn +. ubfn)
    in
    let bfscore = fscore bprec brecl 
    in
    let () = 
      if (counter mod modvalue = 0) 
      then print_endline ((string_of_int counter)^"\t"
			 ^(string_of_float wprec)^"\t"
			 ^(string_of_float wrecl)^"\t"
			 ^(string_of_float wfscore)^"\t"
			 ^(string_of_float bprec)^"\t"
			 ^(string_of_float brecl)^"\t"
			 ^(string_of_float bfscore))
      else ()
    in
    (new_wtp +. uwtp, 
    new_wfp +. uwfp, 
    new_wfn +. uwfn,
    new_btp +. ubtp, 
    new_bfp +. ubfp, 
    new_bfn +. ubfn, counter+1, array)
  else
    let () = Array.set array counter (uwtp,uwfp,uwfn,ubtp,ubfp,ubfn) 
    in
    (wtp +. uwtp, 
    wfp +. uwfp, 
    wfn +. uwfn, 
    btp +. ubtp, 
    bfp +. ubfp, 
    bfn +. ubfn, counter+1, array)


let evaluate testfile goldfile blocksize =
  let () = print_endline ("\nblock\tWP\tWR\tWF\tBP\tBR\tBF") 
  in
  let counter = 0 and
      a = Array.make blocksize (0.,0.,0.,0.,0.,0.) 
  in
  let (wtp,wfp,wfn,btp,bfp,bfn,c,ar)= 
    double_fold (update_scores blocksize) testfile goldfile (0.,0.,0.,0.,0.,0.,counter,a) 
  in
  let wprec = precision wtp wfp in
  let wrecl = recall wtp wfn in
  let bprec = precision btp bfp in
  let brecl = recall btp bfn in
  let wfscore = fscore wprec wrecl in
  let bfscore = fscore bprec brecl in
  print_endline ((string_of_int c)^"\t"
			 ^(string_of_float wprec)^"\t"
			 ^(string_of_float wrecl)^"\t"
			 ^(string_of_float wfscore)^"\t"
			 ^(string_of_float bprec)^"\t"
			 ^(string_of_float brecl)^"\t"
			 ^(string_of_float bfscore))
  
let main () = 
  if Array.get cmd_array err_pos = "true" 
  then let () = usage () in 
    failwith "Unrecognized arguments. See documentation."
  else 
    evaluate 
      (Array.get cmd_array tst_pos)
      (Array.get cmd_array gld_pos)
      (int_of_string (Array.get cmd_array blk_pos))
;;

main ();;

