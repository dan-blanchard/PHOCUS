(** 
    This is documentation for the command line executable `check.

    author: Jeff Heinz
 
    last updated: December 1, 2007

*)

(**

   This page explains how to use the [check] command line executable which
   measures the precision and recall of segmented text against the gold target
   text. The output is three columns printed to standard out: line no.,
   precision, recall (columns are separated by tabs). 

   Precision and recall are measured in the following manner (see reference
   below for details): Each word in the algorithmic segmentation was labeled a
   true positive if it lines up exactly with a word in the gold
   segmentation---that is, both boundaries match. Each word in the algorithmic
   segmentation which does not align exactly with a word in the standard
   segmentation is counted as a false positive. Each word in the standard
   segmentation which does not align exactly with a word in the algorithmic
   segmentation counts as a false negative. Then:
   
   precision = true positives / (true positives + false positives)
   
   recall = true positives / (true positives + false negatives)

   - Brent, Michael. 1999. An Efficient, Probabilistically Sound
   Algorithm for Segmentation and Word Discovery. Machine Learning 34, 71-105.

*)

 (** {3 Usage} *)

(** {2 Invocation} *)

 (** Like other OCaML executables, the program is run by typing [./segment] or
     [ocamlrun segment] at the command prompt.  Generally it is invoked as follows
     (The [./] prefix is omitted for readability).

     [check] {i options} {i testfile} {i goldfile}

     The [testfile] is the algorithmically segmented text. The [goldfile] is
     the target segmented text. By default, precision and recall are computed
     in 500 line chunks. (This can be adjusted as desired with the option [-b]
     (see below).

 *)



(** {2 Options} *)

(** There are three options.  
    - [-sd segment_delimiter] allows you to specify
      how the segments are delimited. The default is the empty string [""].  
    - [-wd segment_delimiter] allows you to specify how the words are
      delimited. The default is a single space [" "].  
    - [-b blocksize] allows you to specify the size of the chunks of the file
    which are evaluated for precision and recall.
*)
