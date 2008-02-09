(* 
   This file contains some code showing how the family of X modules can be
   used. First we create a module called Phone which is of X.X_TYPE.
*)

module Phone = 
struct
  let name = "Phoneme"
  type t = string
  let compare = compare
  let pair x y = y
  let of_string x = x
  let to_string x = x
    
  let print ?oc:(oc=stdout) x = 
    output_string oc (to_string x);
    output_string oc "\n";
    flush oc
      
  let print_ ?oc:(oc=stdout) x = 
    output_string oc (to_string x);
    flush oc
end


(* One of the main goals was to make it easy to get and print abstract types
   using string representations.  

   Now suppose we want to create words (i.e. lists of phonemes). We need to
   know only how to delimit the phones.  First we will use the # mark as a
   word boundary with no space spearating phones *)

module Word = Xlist.Make(Delim.No_space_wbs)(Phone);;

(* Now we can get words from strings immediately. *)

let w = Word.of_string "#hello#";;

(* Suppose we decide that the word boundaries are unnecessary. We just make
   the module again without them *)

module Word = Xlist.Make(Delim.None)(Phone);;

let w = Word.of_string "hello";;

(* Perhaps we need spaces separating the phonemes. *)

module Word = Xlist.Make(Delim.Space_no_bs)(Phone);;

let w = Word.of_string "hello";;
let w = Word.of_string "h e l l o";;
let w = Word.of_string "HH AH0 L OW1";; (* the CMUDICT pronunciation of hello *)

(* Note the empty list is not created by "". Instead the string "<lambda>" is
   reserved for this purpose. This allows up to avoid any potentional
   ambiguity with whitespace.*)

let w = Word.of_string "";;
let w = Word.of_string "<lambda>";;

(* Now lets get sets of words. We'll use curly braces and commas to seperate them. *)

module WordSet = Xset.Make (Delim.Comma_cbs)(Word);;

let ws = WordSet.of_string "{h e l l 0, h u n g r y, m i t t e n,<lambda>}";;
WordSet.print ws;;

(* If we want to get the empty set we can do this in two ways. One is to use a
   string reserved for this: "<emptyset>". The other is, if we have specified
   both a left and right brace, then we can leave nothing in between
   them. Note that both braces must be identified. *)

let ws1 = WordSet.of_string "<emptyset>";;
WordSet.print ws1;;

let ws2 =  WordSet.of_string "{}";;
WordSet.print ws2;;

(* As you can see they will always be written as "<emptyset>".

   Because xset and xlist are X_TYPES themselves it is easy to build sets of
   sets and sets of lists and lists of sets and lists of lists.

   Finally all the set functions are part of modules of XSET_TYPE. Not all
   list functions are part of modules of XLIST_TYPE but most useful ones
   are. There are also some extra functions like prefixes (below) and
   suffixes. *)

let prefixes_w = Word.prefixes (Word.of_string "HH AH0 L OW1");;

let prefix_set = WordSet.of_list prefixes_w;;

WordSet.print prefix_set;;
