(** 

    The Latex module offers some functions to facilitate writing LaTeX documents
    with OCaML.

    author : Jeff Heinz
    last updated : July 15, 2006 

*)


(** Returns a string of the standard preamble with the title of the document
    as an argument. *)
val preamble : string -> string         (* title -> preamble *)

(** Returns a string with of the beginning of a tabular environment.  Usage:
    [btabular pos format]. *)
val btabular : string -> string -> string

(** Returns the string ["\end\{tabular\}"]. *)
val etabular : string

(** [figure caption fig] returns a string which centers the figure [fig]
    with caption [caption]. *)
val figure : string -> string -> string (* caption -> figure -> centered figure *)

(** Returns the string ["\includegraphics\{"^string^"\}"]. *)
val graphics : string -> string         (* \includegraphics{STRING} *)

(** Returns the string ["\end\{document\}"] *)
val close : string                      (* \end{document} *)
