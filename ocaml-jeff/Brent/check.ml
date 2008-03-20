
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
   6 - records whether there was an error
*)

let a_size = 7;;

let seg_pos = 0;;
let wrd_pos = 1;;
let utt_pos = 2;;
let tst_pos = 3;;
let gld_pos = 4;;
let blk_pos = 5;;
let err_pos = 6;;


let cmd_array = Array.make a_size "";;
Array.set cmd_array wrd_pos " ";; (* default word delimiter is space *)
Array.set cmd_array utt_pos "$";; (* default utterance boundary is $ *)
Array.set cmd_array blk_pos "500";; (* default blocksize is 500 *)

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

    | testfile::(goldfile::[]) -> 
	let () = Array.set cmd_array tst_pos testfile in
	Array.set cmd_array gld_pos goldfile 

    | _ -> Array.set cmd_array err_pos "true" ;;

process_args cmd_list;;

let seg_delim = Array.get cmd_array seg_pos;; (*delimits segments within a word*)
let wd_delim = Array.get cmd_array wrd_pos;; (*delimits words from each other*)
let ub_delim = Array.get cmd_array utt_pos;; (*delimits utterances from each other*)

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
    | ([],[]) -> (Letter(wb),Letter(wb))::list                 (* if we have finished the recursion *)
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


let rec true_positives pair_list tp possible_word = 
  match pair_list with
    |  [] -> tp
    | head::tail ->	
	match head with
	  |  (Letter(x),Letter(y)) when (x=wb) && (y=wb) -> 
	       if possible_word 
	       then true_positives tail (tp +. 1.) true 
	       else true_positives tail tp true 
	  | (Blank,_) 
	  | (_,Blank) -> true_positives tail tp false
	  | _ -> true_positives tail tp possible_word (*maintain status quo*);;
  

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
    let () = print_endline ("test as word: "^(Mstring.concat_list Seg_D.delim (List.map Seg.to_string test_list) "")) in
    let () = print_endline ("gold as word: "^(Mstring.concat_list Seg_D.delim (List.map Seg.to_string gold_list) "")) in
  *)
  let pair_list = make_pair_list test_list gold_list [] in
  (*
    let () = print_endline (Mstring.concat_list "\n" (List.rev (List.map (fun (a,b) -> (sym_to_string a)^","^(sym_to_string b)) pair_list)) "")
    in
  *)
  let tp = true_positives pair_list 0. false in
  let fp = (float_of_int (Utterance.length test_u)) -. tp in
  let fn = (float_of_int (Utterance.length gold_u)) -. tp in
  (*  let () = print_endline ((string_of_float tp)^"\t"^(string_of_float fp)^"\t"^(string_of_float fn)) 
      in
  *)
  (tp,fp,fn);;
  

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


let update_tp_fp_fn blocksize test_string gold_string (tp,fp,fn,counter,array) =
  let test_u = Utterance.of_string test_string in
  let gold_u = Utterance.of_string gold_string in

  let utterance_tp,utterance_fp,utterance_fn = 
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
    let c_tp,c_fp,c_fn = Array.get array position
    in
    let new_tp,new_fp,new_fn = tp-.c_tp, fp-.c_fp, fn-.c_fn
    in
    let () = Array.set array position (utterance_tp,utterance_fp,utterance_fn) 
    in
    let prec = precision (new_tp +. utterance_tp) (new_fp +. utterance_fp)
    in
    let recl = recall (new_tp +. utterance_tp) (new_fn +. utterance_fn)
    in
    let () = print_endline ((string_of_int counter)^"\t"^(string_of_float prec)^"\t"^(string_of_float recl))
    in
    (new_tp +. utterance_tp, new_fp +. utterance_fp, new_fn +. utterance_fn, counter+1, array)
  else
    let () = Array.set array counter (utterance_tp,utterance_fp,utterance_fn) 
    in
    (tp +. utterance_tp, fp +. utterance_fp, fn +. utterance_fn, counter+1, array);;  


let evaluate testfile goldfile blocksize =
  let counter = 0 and
      a = Array.make blocksize (0.,0.,0.) 
  in
  double_fold (update_tp_fp_fn blocksize) testfile goldfile (0.,0.,0.,counter,a);;
 


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

