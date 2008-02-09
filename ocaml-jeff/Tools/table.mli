(** 

    This module contains functions for making and using tables implemented as
    a string matrix (string array array). A table can be read from a file. It
    is assumed that the first row contains the column names and the items in
    the first column contain the row names.

    Author: Jeff Heinz.

    Last Updated: June 8, 2006.
    
*)

(** The module Table implements a table as a [string array array].  I may
    switch this to a map in the future. Type [t] is [string array array].*)
type t 

(** [make ?col_delim sl] make makes a table from a string list (representing
    rows) where the cols are delimited in each row by the [col_delim]. If
    [col_delim] is not specified, ['\t'] is used.*)
val of_stringlist :  ?col_delim:char -> string list -> t

(** This function turns a file into a table.  If
    [col_delim] is not specified, ['\t'] is used.*)
val of_file : ?col_delim:char -> string -> t
  (** Usage: of_file filename col_delimiter *)


 (** This function turns a table into a file.  If
    [col_delim] is not specified, ["\t"] is used.*)
val to_file : ?col_delim:string -> t -> string -> unit
  (** Usage: to_file filename table  *)


(** [get_col_names t] returns a list of the column names. *)
val get_col_names : t -> string list
  (** The col names are the elements in the first row excluding the
      first column *)

(** [get_val_pos x y table] returns the string in the [x]th row and [y]th
    column of the array where the first row and column (the row and column
    names) are the zeroth row and column, respectively.   *)
val get_val_pos : int -> int -> t -> string

(**  [get_row_names t] returns a list of the row names. *)
val get_row_names : t -> string list
  (** The row_names are the first element in each row after the
     first row *)

(** [get_val table row_name col_name rowFailure colFailure] takes a
  table, a row name, a col name and two strings to
  indicate how a failure should be specified and returns the value
  of the table at that cell or returns some error message if no such
  row name or col name exist *)
val get_val : t -> string -> string -> string -> string -> string 
