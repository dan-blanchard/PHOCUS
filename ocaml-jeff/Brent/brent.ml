(* This program implements the Dynamic Programming Algorithm for    *)
(* Probabilistic Utterance Segmentation as described by Brent 1999. *)

(* full citation: *)
(* Brent, Michael. 1999. An Efficient, Probabilistically Sound Algorithm *)
(* for Segmentation and Word Discovery. Machine Learning 34, 71-105.     *)

(* Program authors:  Jeff Heinz and Sarah VanWagenen (UCLA) *)


module type BRENT_TYPE = 
sig
  type word
  val word_of_string : string -> word
  val word_to_string : word -> string
  type utterance
  val utterance_of_string : string -> utterance
  val utterance_to_string : utterance -> string
  type lexicon 
  val empty_lexicon : lexicon
  val print_lexicon : ?oc:out_channel -> lexicon -> unit
  val lexicon_to_file :  string -> lexicon -> unit
  val lexicon_of_file : string -> lexicon
  val mdbp1 : word -> lexicon -> bool -> utterance
  val segmenter : ?msg:bool -> ?infile:string -> lexicon -> unit
end

module Make
  (Seg:X.X_TYPE)
  (Seg_D:Delim.DELIM_TYPE)
  (Word_D:Delim.DELIM_TYPE) 
  (Utt_D:Delim.DELIM_TYPE) = 

struct

  (* the word boundary *)  
  let wb = Seg.of_string (Word_D.delim)

  module SegSet = Set.Make(Seg)
  module Word = Xlist.Make(Seg_D)(Seg)

  (* the utterance boundary *)
  let ub = Word.of_string (Utt_D.delim)
  let ub_string = Seg_D.delim^Utt_D.delim

  module Delim_word = struct 
    let delim = (Seg_D.delim)^(Word_D.delim)^(Seg_D.delim)
    let rb = (Word_D.rb)
    let lb = (Word_D.lb)
  end

  module Utterance = Xlist.Make(Delim_word)(Word);;

  type word = Word.t

  let word_of_string = Word.of_string
  let word_to_string = Word.to_string

  type utterance = Utterance.t

  let utterance_of_string = Utterance.of_string
  let utterance_to_string u = (Utterance.to_string u)^(Seg_D.delim)^(Word_D.delim)

  module WordSet = Set.Make(Word);;

  (* The lexicon in Brent's algorithms are implemented as maps. Each word is
     associated with a float. *)
  module Float =
  struct 
    let name = "float"
    type t = float
    let compare = compare
    let pair x y = x *. y
    let of_string = float_of_string
    let to_string = string_of_float
    let print ?oc:(oc=stdout) x = output_string oc (to_string x)
    let print_ = print
  end ;;

  module SegMap = Map.Make(Seg);;

  module Lexicon = Xmap.Make(Word)(Float)(Delim.Mp);;

  type lexicon = Lexicon.t
      
  (* Some convenient functions for maps   *)
  let empty_lexicon = Lexicon.empty
  let print_lexicon = Lexicon.print
  let map_size = Lexicon.size
  let lexicon_to_file = Lexicon.to_file
  let lexicon_of_file = Lexicon.of_file
    
  let sum_values map = 
    Lexicon.fold
      (fun k d a -> d +. a)
      map 
      0. 
      
  let get_value_segmap seg segmap =  
    try SegMap.find seg segmap
    with Not_found -> 0. 
      
  let get_freq_lexicon word lexicon =  
    try Lexicon.find word lexicon
    with Not_found -> 0. 

  let increase_frequency_lexicon map word = 
    (* increase the frequency of a word in the lexicon by 1
       if that word already exists in the lexicon, otherwise
       adds word to lexicon with frequency 1 *)
    try let freq = Lexicon.find word map in
      Lexicon.add word (freq +. 1.) map
    with Not_found -> Lexicon.add word 1. map
	
  let increase_frequency_segmap map seg = 
    (* as above but for the segmap *)
    try let freq = SegMap.find seg map in
      SegMap.add seg (freq +. 1.) map
    with Not_found -> SegMap.add seg 1. map
	
  (* A lexicon is a map: word -> float *)
  (* A lexicon maps words to frequencies *)
  (* We use floats instead of integers because computations will *)
  (* involve floats. *)

  (* There is also a segment map : seg -> float *)
  (*  Brent p. 86: "one occurance of the phoneme is  *)
  (* counted for each word type it appears in, not each token *)
  (* since we are interested in the probabilities of the phoneme  *)
  (* strings in the lexicon."   *)

  (* Segments within words can be delimited by user choice *)
  (* Thus we can use bigraphemes by setting delim to " " *)
  (* for example.  Then ACCENT =  "AH0 K S EH1 N T" using CMUDICT *)
  (* pronuncication symbols *)
	
  let word_to_segset word segset = 
    Word.fold_left
      (fun ss seg -> SegSet.add seg ss)
      segset
      word
      
  let get_segs lexicon = 
  (* returns all the symbols used in a lexicon *)
    Lexicon.fold 
      (fun k d sigma -> word_to_segset k sigma)
      lexicon
      SegSet.empty

  let update_freqSegMap_w w freq_seg_map =
    Word.fold_left 
      (fun fsm seg -> increase_frequency_segmap fsm seg) 
      freq_seg_map w

  let make_freq_segmap lexicon = 
    (* makes a map which records the token frequency of phones *)
    Lexicon.fold
      (fun k d m -> update_freqSegMap_w k m)
      (Lexicon.remove ub lexicon) (* removes utterance boundary from lexicon *)
      SegMap.empty      

  let update_lexicon utterance lexicon =
    (* adds the words in an utterance to some lexicon, updating frequencies *)
    Utterance.fold_left
      (fun lex w -> increase_frequency_lexicon lex w)
      lexicon
      utterance

  let get_total_types lexicon =
    float (Mmap.size lexicon Lexicon.fold)

  let update_total_tokens u tokens =
    (* adds 1 to tokens for every word in the utterance *)
    tokens +. (float (Utterance.length u))

  let update_freqSegMap u freq_seg_map =
    (* adds each occurence of a phoneme in the utterance to the segmap *)
    Utterance.fold_left
      (fun fsm1 w -> update_freqSegMap_w w fsm1) 
      freq_seg_map u 

  let print_fsm fsm = 
    let print_kd key data = 
      print_endline ((Seg.to_string key)^"\t"^(string_of_float data))
    in
    SegMap.iter print_kd fsm 

  let update_total_segs_w w total_segs = 
    (* adds 1 to total segments for every segment in a word in the
       utterance *)
    total_segs +. (float (Word.length w))

  let update_total_segs u total_segs = 
    Utterance.fold_left
      (fun tsgs w -> update_total_segs_w w tsgs) total_segs u

  let freq_map_to_prob_map freq_map total_segs= 
    (* Converts a map of phoneme frequencies to a *)
    (* map: seg -> probability = frequency/totalphones *)
    SegMap.fold
      (fun k d m -> SegMap.add k (d /. total_segs) m)
      freq_map
      SegMap.empty

(*   let update_probSegMap freq_seg_map total_segs = *)
(*     freq_map_to_prob_map freq_seg_map total_segs *)

  let get_total_segs freq_segmap = 
    (* count number of symbols given a map from segments to frequency *)
    SegMap.fold 
      (fun k d a -> d +. a)
      freq_segmap 0.
 
  let prob_word word freq_seg_map total_segs = 
    (* computes the probability of a word given the probability
       distribution over the symbols *)
    let prob_seg seg = 
      (get_value_segmap seg freq_seg_map) /. total_segs
    in
    let wordprob = Word.fold_left 
	(fun p seg -> p *. (prob_seg seg)) 1. word
    in 
(*    let excluding_empty_word = 1. /. (1. -. (prob_seg wb))  
    in*)
    wordprob (* *. excluding_empty_word*)
      
      
  let prob_all_words lexicon freq_seg_map total_segs =
    (* computes the probability of the lexicon given the probability
       distribution over the symbols *)
    Lexicon.fold
      (fun k d a -> (prob_word k freq_seg_map total_segs) +. a)
      lexicon
      0.
      
  let third_term lexicon freq_seg_map word types total_segs= 
    (* this is the 3rd term in in (21) pg 87 of Brent 1999*)
    (prob_word word freq_seg_map total_segs) (* /. 
          (1. -. (((types -. 1.)/. types) *. 
    	       (prob_all_words lexicon freq_seg_map total_segs)) )  *)
      
  let r_familiar word_frequency total_tokens = 
    (* computes (22) on pg 87 of Brent 1999 *)
    (word_frequency/. total_tokens) *. 
     ( ((word_frequency-.1.) /. word_frequency ) ** 2.)
          
  let pi = 3.1415926535897932
  let dist_pos_int = 6. /. pi**2. 
      
  let r_novel word lexicon freq_seg_map total_types total_tokens total_segs = 
    (* computes (21) on pg 87 of Brent 1999 *)
    dist_pos_int *. (total_types /. total_tokens) *. 
      (third_term lexicon freq_seg_map word total_types total_segs) *. 
      (((total_types -. 1.)/. total_types)**2.) 
      
  let calculate_r word lexicon freq_seg_map total_types total_tokens total_segs =
    (* computes R in the MDBP-1 algorithm in figure 2 on page 91 of
       Brent 1999 *)
    let word_frequency = get_freq_lexicon word lexicon
    in 
    let new_total_types = 
      if word_frequency >= 1. (* if novel word it should occur exactly once in the lexicon *)
      then total_types +. 1.
      else total_types
    in
    let outing = false in (* set to true for debugging *)
    let () = 
      if outing then begin
	  print_endline ("considering word... "^(Word.to_string word));
	  print_endline ("CALCULATING R--ARGUMENTS");
	  print_endline "---LEXICON---";
	  Lexicon.print lexicon; print_endline "---segment frequency---";
	  print_fsm freq_seg_map;      print_endline "---";
	  print_string ("\nTotal word types = "^(string_of_float new_total_types));
	  print_string ("\nTotal word tokens = "^(string_of_float total_tokens));
	  print_string ("\nTotal segments = "^(string_of_float total_segs))
	end      else ()
    in
    if word_frequency > 1. (* if novel word it should occur exactly once in the lexicon*)
    then 
      r_familiar word_frequency total_tokens 
    else 
      r_novel word lexicon freq_seg_map new_total_types total_tokens total_segs

  (* some useful printing functions for debugging the MDBP-1 algorithm 
     which uses arrays *)

  let print_array a elt_to_string title =
    let list = Array.to_list a in
    let sl = List.map elt_to_string list in
    print_endline (title^"\n"^(Mstring.concat_list " | " sl "")) 
      
  let print_arrays (bp,bs) = 
    let () = print_array bp string_of_float "Best Product" in
    let () = print_array bs string_of_int "Best Start" in
    ()
      
  let print_chars fc lc =  
    let () = print_newline () in
    print_string ("first_char: "^(string_of_int fc)^
		   " last_char: "^(string_of_int lc))
  let print_word w = 
    print_endline (" subword: "^(Word.to_string w))

  let print_scores ws iv = 
    print_endline ("word_score: "^(string_of_float ws)^
		    " important_value: "^(string_of_float iv))

  let recover_word array first_pos last_pos add_wb =
    (* recover_word returns a word (with a word boundary if add_wb is true)
       from first_pos and las_pos in the array *)
    let rec helper position word =
      if position > last_pos 
      then word
      else 
	let seg = Array.get array position in
	helper (position+1) (Word.build seg word)
    in 
    if add_wb 
    then Word.rev (Word.build wb (helper first_pos Word.empty))
    else Word.rev (helper first_pos Word.empty)
      
  (* MDBP-1 is the algorithm given in figure 2 on page 91 of Brent 1999. *)
  (* We follow his algorithm closely using two arrays, best_product and *)
  (* best_start. We replace the nested for-loops with an equivalent *)
  (* recursive structure called outer_loop and inner_loop. The outer_loop *)
  (* advances last_char and the inner_loop advances *)
  (* first_char. Insert_boundaries inserts the word boundaries. *)
      
  (* MDBP-1 takes a list of segments (with no word boundaries hence      *)
  (* "utterance_word" is of type Word.t), a lexicon, and returns a list of *)
  (* words, somthing of type Utterance.t. *)
      
  (* Note also that we use an utterance_array. This is because the symbol *)
  (* delimiter may not necessarily be the empty string, e.g. it could be *)
  (* a space " ". *)


  let mdbp_1 utterance_word lexicon freq_seg_map total_types total_tokens total_segs outing  = 
    
    let u_array = Array.of_list utterance_word
    in
    let ulength = Array.length u_array
    in
    let best_product = Array.make ulength 0.
    in
    let best_start = Array.make ulength 0 
    in
    let rec inner_loop first_char last_char = 
      if first_char > last_char                       (* the stopping condition *)
      then if outing then print_newline () else ()
      else 
	let subword = recover_word u_array first_char last_char true
	in 
	let score = calculate_r 
	  subword 
	  (increase_frequency_lexicon lexicon subword)
	  (update_freqSegMap_w subword freq_seg_map) 
	  (total_types) 
	  (total_tokens +. 1.) 
	  (update_total_segs_w subword total_segs)
	in
	let word_score =
	  if score<>score then 0. else score (* checks whether score is nan *)
	in
	(*	let () = print_string (string_of_float word_score) in  *)
	let important_value = 
	  word_score *. (Array.get best_product (first_char - 1)) 
	in
	let () = 
	  if important_value > (Array.get best_product last_char)
	  then 
	    let () = if outing 
	      then print_endline "--better important_value--"
	      else ()
	    in
	    let () = Array.set best_product last_char important_value 
	    in
	    Array.set best_start last_char first_char 
	  else ()
	in
	let () = if outing 
	  then 
	    let () = print_chars first_char last_char in
	    let () = print_word subword in 
	    let () = print_scores word_score important_value in
	    print_arrays (best_product,best_start) 
	  else ()
	in
	inner_loop (first_char+1) last_char
    in 
    let rec outer_loop last_char =
      if last_char > (ulength-1)
      then let () = print_newline () in ()
      else 
	let subword = recover_word u_array 0 last_char true
	in 
	let score = 
	  let w = subword  in
	  let lex = (increase_frequency_lexicon lexicon subword) in
	  let ufsm = (update_freqSegMap_w subword freq_seg_map) in
	  let ttp = (total_types) in
	  let ttn = (total_tokens +. 1.) in
	  let tsg = (update_total_segs_w subword total_segs) in
	  calculate_r w lex ufsm ttp ttn tsg 
	in 
	let word_score = 
	  if score<>score then 0. else score (*checks whether score is the same as nan*)
	in
	let () = Array.set best_product last_char word_score
	in 
	let () = Array.set best_start last_char 0
	in 
	let () = if outing 
	  then 
	    let () = print_chars 0 last_char in
	    let () = print_word subword in
	    let () = print_scores word_score ~-.1. in
	    print_arrays (best_product,best_start) 
	  else () 
	in
	let () = inner_loop 1 last_char in 
	outer_loop (last_char+1)
    in
    let rec insert_boundaries u first_char =
      if first_char <= 0 then u
      else
	let s_array = Array.of_list (Utterance.hd u)
	in
	let s_length = Array.length s_array
	in
	let rem,word= 
	  (recover_word s_array 0 (first_char-1) false),
	  (recover_word s_array first_char (s_length-1) false)
	in
	let next_fc = first_char-1 in
	let () = if outing 
	  then 
	    let () = print_endline ("first_char: "^ (string_of_int first_char)) in
	    let () = print_endline ("rem : "^(Word.to_string rem)) in
	    let () = print_endline ("word: "^(Word.to_string word)) in
	    print_endline ("next_fc: "^(string_of_int next_fc))
	  else ()
	in
	insert_boundaries 
	  (Utterance.build rem (Utterance.build word (Utterance.tl u)))
	  (Array.get best_start (first_char-1))
    in
    (* main begins here *)
    let () = outer_loop 0 in
    let () = if outing then print_endline "\n---inserting boundaries..." else () 
    in
    let best_start_for_last_point = Array.get best_start (ulength-1) 
    in
    insert_boundaries 
      (Utterance.build utterance_word Utterance.empty)
      best_start_for_last_point
      

  let mdbp1 u lexicon msg =
    let fsm0 = make_freq_segmap lexicon in
    let tsgs0 = get_total_segs fsm0 in
    let typs0 = float (Lexicon.size lexicon) in
    let tkns0 = sum_values lexicon 
    in 
    mdbp_1 u lexicon fsm0 typs0 tkns0 tsgs0 msg
    

  (* Each utterance should be on its own line.  Empty lines will be ignored,
     as will lines whose first character is '%' (allowing for commenting of
     the file).  

     currently dismanled this feature. Plan to make this an option that can 
     be passed in 

  *)


  let file_fold f inchannel a =
    (* f must be a function of type : string -> a -> a *)
    let eof = ref false in
    let aref = ref a in
    while 
      not !eof
    do try
	let line = input_line inchannel in
	if  (line = "") (* || (line.[0] = '%') *)
	then ()
	else aref := f line !aref
      with 
	| End_of_file -> eof := true
    done;
    close_in inchannel;
  !aref


  let segmenter ?msg:(msg=false) ?infile:(infile="") lexicon = 
    
    let f s (lex,fsm,tsgs,typs,tkns) =
      let () = if msg then
	  let () = print_endline ("\n|Total Segs| = "^(string_of_float tsgs)) in
	  let () = print_endline ("|Word Types| = "^(string_of_float typs)) in
	  print_endline ("|Word Tokens| = "^(string_of_float tkns)) 
	else ()
      in
      if  (s = "") (* || (s.[0] = File.comment_char) *)
      then 
	let () = output_string stdout ("\n") in 
	(lex,fsm,tsgs,typs,tkns)
      else
	let u = mdbp_1 (Word.of_string s) lex fsm typs tkns tsgs msg in
	let () = output_string stdout (utterance_to_string u) in
	let u_wb = Utterance.map (fun w -> Word.append w (Word.build wb Word.empty)) u in
	let u1 = Utterance.build ub u_wb in (* adds the utterance boundary symbol to the utterance *)
	(* notice for purposes of updating, it doesn't matter that 
	   the ub doesn't go to the end of the utterance! *)
	let lex1 = update_lexicon u1 lex in
	let fsm1 = update_freqSegMap u_wb fsm in (* here we don't include the ub *)
	let tsgs1 = update_total_segs u_wb tsgs in  (* here we don't include the ub *)
	let typs1 = (get_total_types lex1) -. 1. in  (* subtract one here if you dont want to include the utterance boundary as a type *)
	let tkns1 = update_total_tokens u1 tkns in
	(lex1,fsm1,tsgs1,typs1,tkns1)
    in
    let fsm0 = make_freq_segmap lexicon in
    let tsgs0 = get_total_segs fsm0 in
    let typs0 = (try SegMap.find wb fsm0 with Not_found -> 0.) in (* the one is for the ub *)
    let tkns0 = sum_values lexicon 
    in
    let g lex s = f s lex
    in 
    match infile with
      | "" -> 
	  let sl = Mstring.to_stringlist "\n" (File.read ()) in
	  let final_lex,_,_,_,_ = List.fold_left g (lexicon,fsm0,tsgs0,typs0,tkns0) sl in
	  lexicon_to_file "lexicon" final_lex

      | _ ->
	  let ifile = open_in infile in
	  let final_lex,_,_,_,_ = file_fold f ifile (lexicon,fsm0,tsgs0,typs0,tkns0) in
	  let () = close_in ifile in
	  lexicon_to_file "lexicon" final_lex
  ;;

end
