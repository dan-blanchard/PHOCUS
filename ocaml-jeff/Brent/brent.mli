(** This module implements the Dynamic Programming Algorithm for    
 Probabilistic Utterance Segmentation as described by Brent 1999. 

  Full citation: Brent, Michael. 1999. An Efficient, Probabilistically Sound Algorithm 
  for Segmentation and Word Discovery. Machine Learning 34, 71-105.     

  Authors: Jeff Heinz and Sarah VanWagenen

  Last modified: August 2006

*)



(** The output signature of Make. *)
module type BRENT_TYPE = 
sig

  (** A word is a list of segments. *)
  type word 

  (** Returns a word from its string representation. *)
  val word_of_string : string -> word

  (** Returns the string representation of a word. *)
  val word_to_string : word -> string

  (** An utterance is a list of words. *)
  type utterance

  (** Returns an utterance from its string representation. *)
  val utterance_of_string : string -> utterance

  (** Returns the string representation of an utterance. *)
  val utterance_to_string : utterance -> string

  (** A lexicon is map from words to floating point values which, in MDBP-1,
      represent frequency values. *)
  type lexicon 

  (** The empty lexicon. *)
  val empty_lexicon : lexicon
    
  (** [print_lexicon lex] prints a lexicon to output channel [oc]. [oc] is
      optional (default is standard output). *)
  val print_lexicon : ?oc:out_channel -> lexicon -> unit
    
  (** [lexicon_to_file filename lex] takes a lexicon and writes it to a file.*)
  val lexicon_to_file :  string -> lexicon -> unit

  (** [lexicon_of_file filename] takes a file with words associated with
      floating point numbers and returns a lexicon. Key (words) and data
      (number) values in the file must be separated by a single tab '\t'. *)
  val lexicon_of_file : string -> lexicon
    
  (** MDBP-1 is the algorithm given in figure 2 on page 91 of Brent 1999.
      [mdbp1 word lexicon msg] takes a list of segments (with no word
      boundaries), and a lexicon, and returns a segmented utterance, that is a
      list of words. If [msg] is set to [true] then MDBP-1 will print each stage
      of its processing. (This produces a lot of output so use with care.)  *)
  val mdbp1 : word -> lexicon -> bool -> utterance

  (** Usage: [segmenter ?msg ?infile lexicon] segments a file of unsegmented
      utterances and writes the segmented utterances to the outchannel.  There
      are two optional arguments.  If no infile is given to the function,
      utterance are given via standard input.  If no [msg] is given to
      [segment] the default is [false]. Segmented utterances are written to
      standard output. This function writes the lexicon that exists after
      segmenting all the utterances to the file "lexicon".*)

  val segmenter : ?msg:bool -> ?infile:string -> lexicon -> unit

end


(** The input signature to Make. *)
module Make
  (Seg:X.X_TYPE)
  (Seg_D:Delim.DELIM_TYPE)
  (Word_D:Delim.DELIM_TYPE) :  BRENT_TYPE


