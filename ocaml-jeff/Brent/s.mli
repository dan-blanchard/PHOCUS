(** S implements the phoneme-level segments in an utterance as strings. The S
    module is of type [X.X_TYPE].

    author: Jeff Heinz

    last updated : August 2006
*)

include X.X_TYPE
    (** The following list summarizes the functions in modules of [X.X_TYPE]
	which is described in the Tools documentation.  
	- Type [t] is a string,i.e. nodes are implemented over strings.  
	- [name] is ["Segment"].  
	- [compare] is the same as [Pervasives.compare].  
	- [pair s1 s2] returns a pair [(s1,s2)].  
	- [of_string] returns a segment from its string representation.  
	- [to_string] returns a string representation of a segment.  
	- [print] prints a segment followed by an endline.  
	- [print_] prints a segment but no endline.
*)

