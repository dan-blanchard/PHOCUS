
#load "../brent.cma";;

module B = Brent.Make
  (S)                   (* Implements segments as strings *)
  (Delim.None)          (* Sets the segment delimiter to the empty string *)
  (Delim.Space_no_bs)   (* Sets the word delimiter to a space *)
;;

let lex = B.lexicon_of_file "lex";;
B.print_lexicon lex;;

let no_msg = false;; (* only segmented utterances are output *)
let msg = true;; (* daignostic messages are also output *)

let u1 = B.mdbp1 (B.word_of_string "abbcdbba") lex msg;;
B.utterance_to_string u1;;

let u1 = B.mdbp1 (B.word_of_string "abbcdbba") lex no_msg;;
B.utterance_to_string u1;;

let u2 = B.mdbp1 (B.word_of_string "abbcdbbab") lex no_msg;;
B.utterance_to_string u2;;


module C = Brent.Make
  (S)                   (* Implements segments as strings *)
  (Delim.Space_no_bs)   (* Sets the segment delimiter to space *)
  (Delim.Hash_no_bs)    (* Sets the word delimiter to "#" *)
;;


let lex2 = C.lexicon_of_file "lex2";;
C.print_lexicon lex2;;

let w1 = C.word_of_string "a c b b dz a b b dz a a";;
let v1 = C.mdbp1 w1 lex2 no_msg;;
C.utterance_to_string v1;;


(* The algorithm doesn't always behave as you might expect. Observe: *)

let w2 = C.word_of_string "a b b c d b b a";;
let v2 = C.mdbp1 w2 lex2 no_msg;;
C.utterance_to_string v2;;


(* See the file 'cmd_line' for more examples related to the behavior of the
algorithm.  *)
