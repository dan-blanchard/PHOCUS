(** 
  The Mstring module contains more functions for manipulating strings, characters and
  lists of those strings, characters etc.
 
  Authors: Jeff Heinz and Greg Kobele. 
1
  Last updated:  May 4, 2006.

*)


(** [tokenize s] converts string s to a char list. *)
val tokenize : string -> char list
  (** E.g. tokenize "abc" returns \['a';'b';'c'\]. *)

(** [break_at c s] takes a char and a string and returns a string list with char
     by breaking the string at char locations. *)
val break_at : char -> string -> string list
  (** E.g. break_at '.' "ab.cd.ef" returns \["ab";"cd";"ef"\]. *)

(** Returns the number of instances of [char] in [string]. *)
val count_char : char -> string -> int

(** [to_stringlist delim s] is like break_at except takes a string 
  instead of a char.  If delim is "", breaks string at every char. *)
val to_stringlist : string -> string -> string list

(** [print_list sl] prints to standard output each string of the list on
  its own line. *)
val print_list : string list -> unit

(** [split_at n s] takes an int and a string and returns a pair of strings
    separated at int. *)
val split_at : int -> string -> string * string

(** [concat_list delimiter sl s] concats sl with the delimiter, which is
    concatenated to suffix s. *)
val concat_list : string -> string list -> string -> string 
  (** E.g. concat_list " " \["the";"boy";"ran"\] "." returns "the boy ran." *)

(** [remove_char c s] removes all instances of char c in s. *)
val remove_char : char -> string -> string

(** [remove_chars cl s] removes every char in char list cl that is
    found in s. *)
val remove_chars : char list -> string -> string

(** Returns the string with no spaces, newlines, or tab characters. *)
val remove_whitespace : string -> string

(** [extractBetweenDelimiters s lbrace rbrace pos] scans rightward a string s
    beginning at pos and extracts a string which exists between the first
    occurrence of lbrace and of rbrace. It returns a pair: the extracted
    string and the next position in the string. *)
val extractBetweenDelimiters :
  string -> string -> string -> int -> string * int
  (** E.g. extractBetweenDelimiters "1,2,<a,b>,3" "<" ">" 0 returns ("a,b", 9). *)

(** [to_pair c s] is like split_at but takes the first instance of char as the
  splitting point and excludes char. *)
val to_pair : char -> string -> string * string
