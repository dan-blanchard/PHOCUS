(** 

  This file contains functions for reading and writing to files and directories.

  Author: Jeff Heinz.

  Last Updated: May 8, 2006

*)


(**
  NOTE: Empty lines and those whose first charcter is '%' are
  ignored. Thus comments may be added to files by using the '%'
  character at the beginning of the line.
*)


(** {3 Reading from files } *)

(** The char used at the beginning of a line to indicate a comment. It is
    currently ['%']. *)
val comment_char: char

(**  [to_stringlist filename] converts a file to a string list. Each line becomes
     a string in the list.  *)
val to_stringlist : string -> string list

(**  [to_strict_stringlist filename] is like to_stringlist but doesn't ignore empty or
  commented lines. *)
val to_strict_stringlist : string -> string list


 (** [to_string filename] converts a file to a string with *NO* delimiter betweeen lines. *)
val to_string : ?delim:string -> string -> string


(** [fold f ic a] computes [(f lineN ... (f line2 (f line1 a))...)] where
    [line1 ... lineN] are lines of the input channel [ic], from first to
    last.*)
val fold : (string -> 'a -> 'a) -> in_channel -> 'a -> 'a

(** Returns a string that the user enters from standard input. Use ^D to
    terminate the input. Alternatively you may enter [<eof>] by itself on its
    own line to terminate the input.*)
val read : unit -> string 


(**  [process filename f] applies [f] to every line
  in the file. *)
val iter : string -> (string -> unit) -> unit


(** {3 Writing to files } *)

 (**  [of_string filename string] makes a file out of a string. *)
val of_string : string -> string -> unit


 (** [of_stringlist filename sl] makes a file out of a string list. Each string is on its own line. *)
val of_stringlist : string -> string list -> unit


 (** [append_to_front s filename] appends a string to the front of a file. *)
val append_to_front : string -> string -> unit


(** [comment_string s] appends [comment_char] to the front of the string. *)
val comment_string : string -> string


(** {3 Operations for file names } *)


(** This function removes offending characters from a string to make a legal
  filename. The following characters are removed: ['.';'?';':';',';'\{';'\}';'\[';'\]']. *)
val name_of_string : string -> string
  (** Usage: name_of_string filename   *)

 (** [name_rm_ext filename] removes anything after the first '.' in the string. *)
val name_rm_ext : string -> string

 (** [name_get_ext filename] returns the portion of the string after the last '.' in the
   string. *)
val name_get_ext : string -> string


(** {3 Operations over directories } *)

 (** [to_ignore filename] returns true iff filename begins with '.' or '#' or ends with
   '~' or is named "not_in_use" (typically a directory). *)
val to_ignore : string -> bool


 (**  [dir_apply f dirname] applies [f] to every file in the
   directory dirname, excluding those which should be ignored by
   to_ignore. *)
val dir_apply : (string -> unit) -> string -> unit


 (** [dir_make filename] makes a directory accessible to all. *)
val dir_make : string -> unit


(** {3 Miscellaneous } *)

 (** [dot2x filename x] turns a dot file into a [x] file where [x] is any of
 the supported filetypes of dot, e.g. ps, gif, png, etc. *)
val dot2x : string -> string -> Unix.process_status


