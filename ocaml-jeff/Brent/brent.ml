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
  (Word_D:Delim.DELIM_TYPE) = 

struct

  (* the word boundary *)
  let wb = Seg.of_string (Word_D.delim)

  module SegSet = Set.Make(Seg)
  module Word = Xlist.Make(Seg_D)(Seg)

  module Delim_word = struct 
    let delim = (Seg_D.delim)^(Word_D.delim)^(Seg_D.delim)
    let rb = (Word_D.rb)
    let lb = (Word_D.lb)
  end

  module Utterance = Xlist.Make(Delim_word)(Word)
    
  type word = Word.t

  let word_of_string = Word.of_string
  let word_to_string = Word.to_string

  type utterance = Utterance.t

  let utterance_of_string = Utterance.of_string
  let utterance_to_string = Utterance.to_string

  module WordSet = Set.Make(Word)
    
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
  end 
    
  module SegMap = Map.Make(Seg)

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
	
  (* we set the delimter for the word to the empty string "" *)
  (* Note that we can use bigraphemes by setting delim to " " *)
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
      
  let make_freq_segmap lexicon = 
    (* makes a map which records the type frequency of phones *)
    let sigma = get_segs lexicon
    in 
    SegSet.fold
      (fun seg segmap ->
	Lexicon.fold
	  (fun k d m -> 
	    if Word.mem seg k
	    then increase_frequency_segmap m seg
	    else m)
	  lexicon
	  segmap
      )
      sigma
      SegMap.empty
      
  let freq_map_to_prob_map freq_map total_segs= 
    (* Converts a map of phoneme frequencies to a *)
    (* map: seg -> probability = frequency/totalphones *)
    SegMap.fold
      (fun k d m -> SegMap.add k (d /. total_segs) m)
      freq_map
      SegMap.empty

  let make_prob_seg_map lexicon = 
    (* from the lexicon, makes a map: symbols -> probabilities *)
    let freq_map = make_freq_segmap lexicon in
    let total_segs = 
      SegMap.fold 
	(fun k d a -> d +. a)
	freq_map 0.
    in
    freq_map_to_prob_map freq_map total_segs
      

  let prob_word word prob_seg_map = 
    (* computes the probability of a word given the probability
       distribution over the symbols *)
    let wordprob = 
      Word.fold_left 
	(fun p seg -> p *. (get_value_segmap seg prob_seg_map))
	1.
	word
    in 
    let excluding_empty_word = 
      1. /. (1. -. get_value_segmap wb prob_seg_map) 
    in
    wordprob *. excluding_empty_word
      
      
  let prob_all_words lexicon prob_seg_map =
    (* computes the probability of the lexicon given the probability
       distribution over the symbols *)
    Lexicon.fold
      (fun k d a -> (prob_word k prob_seg_map) +. a)
      lexicon
      0.
      
  let third_term lexicon prob_seg_map word types = 
    (* this is the 3rd termin in (21) pg 87 of Brent 1999*)
    (prob_word word prob_seg_map) /. 
      (1. -. (((types -. 1.)/. types) *. 
	       (prob_all_words lexicon prob_seg_map)) ) 
      
  let r_familiar word_frequency total_tokens = 
    (* computes (22) on pg 87 of Brent 1999 *)
    (word_frequency/. total_tokens) *. 
      ( ((word_frequency-.1.) /. word_frequency ) ** 2.)
          
  let pi = 3.1415926535897932
  let dist_pos_int = 6. /. pi**2. 
      
  let r_novel word lexicon prob_seg_map total_types total_tokens = 
    (* computes (21) on pg 87 of Brent 1999 *)
    dist_pos_int *. (total_types /. total_tokens) *. 
      (third_term lexicon prob_seg_map word total_types) *. 
      (((total_types -. 1.)/. total_types)**2.) 
      
  let calculate_r word lexicon prob_seg_map total_types total_tokens=
    (* computes R in the MDBP-1 algorithm in figure 2 on page 91 of
       Brent 1999 *)
    let word_frequency = get_freq_lexicon word lexicon
    in 
    if word_frequency > 0. 
    then r_familiar word_frequency total_tokens
    else r_novel word lexicon prob_seg_map total_types total_tokens

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

  let recover_word array first_pos last_pos =
    (* recover_word returns a word (with a word boundary) from first_pos and las_pos in
       the array *)
    let rec helper position word =
      if position > last_pos 
      then Word.rev word
      else 
	let seg = Array.get array position in
	helper (position+1) (Word.build seg word)
    in 
    helper first_pos Word.empty
      
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


  let mdbp_1 utterance_word lexicon prob_seg_map total_types total_tokens outing = 
    
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
	let subword = recover_word u_array first_char last_char
	in 
	let word_score = calculate_r subword lexicon 
	  prob_seg_map total_types total_tokens
	in
	let important_value = 
	  word_score *. (Array.get best_product (first_char - 1)) 
	in
	let () = 
	  if important_value > (Array.get best_product last_char)
	  then 
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
	let subword = recover_word u_array 0 last_char
	in 
	let () = 
	  Array.set best_product last_char 
	    (calculate_r subword lexicon prob_seg_map total_types total_tokens)
	in 
	let () = 
	Array.set best_start last_char 0
	in 
	let () = if outing 
	  then 
	    let () = print_chars 0 last_char in
	    let () = print_word subword in
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
	  (recover_word s_array 0 (first_char-1)),
	  (recover_word s_array first_char (s_length-1))
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
    let () = if outing then print_endline "inserting boundaries..." else () in
    insert_boundaries 
      (Utterance.build utterance_word Utterance.empty)
      (Array.get best_start (ulength-1))


  let mdbp1 u lexicon msg =
    let fsm0 = make_freq_segmap lexicon in
    let tsgs0 = float (Mmap.size fsm0 SegMap.fold) in
    let psm0 = freq_map_to_prob_map fsm0 tsgs0 in
    let typs0 = float (Lexicon.size lexicon) in
    let tkns0 = sum_values lexicon 
    in 
    mdbp_1 u lexicon psm0 typs0 tkns0 msg


  let update_lexicon utterance lexicon =
    (* adds the words in an utterance to some lexicon, updating frequencies *)
    Utterance.fold_left
      (fun lex w -> increase_frequency_lexicon lex w)
      lexicon
      utterance

  let update_total_types u types lexicon =
    (* adds 1 to types for every new word in the utterance *)
    Utterance.fold_left 
      (fun t w -> 
	try let _ = Lexicon.find w lexicon in t
	with Not_found -> t +. 1.
      ) types u

  let update_total_tokens u tokens =
    (* adds 1 to tokens for every word in the utterance *)
    Utterance.fold_left (fun t w -> t+.1.) tokens u 

  let update_freqSegMap u freq_seg_map =
    (* recall that a segment which occurs twice in the same word only counts
       once for segment frequency *)
    Utterance.fold_left
      (fun fsm1 w -> 
	let segset = word_to_segset w SegSet.empty in SegSet.fold
	(fun s fsm2 -> 
	  if Word.mem s w 
	  then increase_frequency_segmap fsm2 s
	  else fsm2)
	segset fsm1
      ) freq_seg_map u 

  let update_total_segs u freq_seg_map total_segs = 
    (* adds 1 to total segments for every new segment in a word in the
       utterance *)
    Utterance.fold_left
      (fun t1 w -> Word.fold_left
	(fun t2 s -> 
	  try let _ = SegMap.find s freq_seg_map in
	      t2
	  with Not_found -> t2 +. 1.
	) t1 w
      ) total_segs u


  let update_probSegMap freq_seg_map total_segs =
    freq_map_to_prob_map freq_seg_map total_segs



  (* Each utterance should be on its own line.  Empty lines will be ignored,
     as will lines whose first character is '%' (allowing for commenting of
     the file).  *)

  let segmenter ?msg:(msg=false) ?infile:(infile="") lexicon = 
  
    let f s (lex,fsm,tsgs,psm,typs,tkns) =
      let () = if msg then
	  let () = print_endline ("|Sigma| = "^(string_of_float tsgs)) in
	  let () = print_endline ("|Word Types| = "^(string_of_float typs)) in
	  print_endline ("|Word Tokens| = "^(string_of_float tkns)) 
	else ()
      in
      if  (s = "") || (s.[0] = File.comment_char)
      then let () = output_string stdout ("\n") in 
	   (lex,fsm,tsgs,psm,typs,tkns)
      else
	let u = mdbp_1 (Word.of_string s) lex psm typs tkns msg in
	let () = output_string stdout ((Utterance.to_string u)^"\n") in
	let lex1 = update_lexicon u lex in
	let fsm1 = update_freqSegMap u fsm in
	let tsgs1 = update_total_segs u fsm tsgs in
	let psm1 = update_probSegMap fsm1 tsgs1 in
	let typs1 = update_total_types u typs lex in 
	let tkns1 = update_total_tokens u tkns in
	(lex1,fsm1,tsgs1,psm1,typs1,tkns1)
    in
    let g lex s = f s lex
    in
    let fsm0 = make_freq_segmap lexicon in
    let tsgs0 = float (Mmap.size fsm0 SegMap.fold) in
    let psm0 = freq_map_to_prob_map fsm0 tsgs0 in
    let typs0 = float (Lexicon.size lexicon) in
    let tkns0 = sum_values lexicon 
    in 
    match infile with
      | "" -> 
	  let sl = Mstring.to_stringlist "\n" (File.read ()) in
	  let final_lex,_,_,_,_,_ = List.fold_left g (lexicon,fsm0,tsgs0,psm0,typs0,tkns0) sl in
	  lexicon_to_file "lexicon" final_lex

      | _ ->
	  let ifile = open_in infile in
	  let final_lex,_,_,_,_,_ = File.fold f ifile (lexicon,fsm0,tsgs0,psm0,typs0,tkns0) in
	  let () = close_in ifile in
	  lexicon_to_file "lexicon" final_lex

end
