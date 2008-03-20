(** 
    This is documentation for the command line executable `segment'.

    author: Jeff Heinz
 
    last updated: July 30, 2006

*)

(**

   This page explains how to use the [segment] command line executable which
   implements the Dynamic Programming Algorithm for Probabilistic Utterance
   Segmentation as described in the following paper.  
   - Brent, Michael. 1999. An Efficient, Probabilistically Sound
   Algorithm for Segmentation and Word Discovery. Machine Learning 34, 71-105.
   
*)

 (** {3 Usage} *)

(** {2 Invocation} *)

 (** Like other OCaML executables, the program is run by typing [./segment] or
     [ocamlrun segment] at the command prompt.  Generally it is invoked as follows
     (The [./] prefix is omitted for readability).

     [segment] {i options} {i file}

     If {i file} is included in the command line, [segment] will operate on
     this file. Each line is assumed to be an utterance with the segments
     delimited by some segment delimiter (See options below).

     If {i file} is not included, then [segment] reads from standard
     input. You can indicate that you have finished standard input by entering
     [<eof>] on a line by itself or by using [^D].

     The segmented utterances are written to standard output. To put them in a
     file, use redirection. E.g.: 

     [segment Alice_in_Wonderland > seg.txt]

 *)



(** {2 Options} *)

(** There are three options.  
    - [-sd segment_delimiter] allows you to specify
      how the segments are delimited. The default is the empty string [""].  
    - [-wd segment_delimiter] allows you to specify how the words are
      delimited. The default is a single space [" "].  
    - [-l lexicon] allows you to specify a file that contains a lexicon to be
    used. Each line in a    lexicon file consists of a word and a frequency
    value (positive integer) seperated by a tab.  
    - [-v] prints to standard output every state the algorithm goes through in
    segmenting each utterance, in addition to the segmented utterance.  Use
    with care as each utterance there is quite a bit of output for each
    utterance.

    Example: 

    [segment -sd " " -wd "#" Alice_in_Wonderland]

    This command will recognize an utterance as a list of segments separated
    by spaces and will insert the [#] mark as word boundaries. This is useful if
    one is using CMUDICT phones for example.

*)
