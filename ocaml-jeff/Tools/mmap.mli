(** 

  Mmap has a few more useful functions for Maps.

  author: Jeff Heinz

  last updated: May 29, 2006

*)


val size : 'a -> (('b -> 'c -> int -> int) -> 'a -> int -> 'd) -> 'd
  (** [size m map_fold] returns the number of elements in map [m] *)


val to_string :
  string ->
  string ->
  'a ->
  (('b -> 'c -> string -> string) -> 'a -> string -> 'd) ->
  ('b -> string) -> ('c -> string) -> 'd
  (** [to_string relation_delim elt_delim m map_fold key_to_string
    data_to_string] converts the map to a string where the [key] and
    its binding [data] are separated by [relation_delim] and each
    element in the map is separated by [elt_delim]. *)

val of_string :
  string -> string -> string ->
  ('a -> 'b -> 'c -> 'c) ->  'c ->
  (string -> 'a) ->
  (string -> 'b) -> 'c
  (** [of_string s relation_delim elt_delim map_add map_empty key_of_string data_to_string] 
      returns a map from a string wherein a key and its binding are delimited
      by [relation_delim] and the elements of the map are delimited by [elt_delim].
  *)

val print :
  ?oc:out_channel ->
  'a ->
  (('b -> 'c -> unit) -> 'a -> 'd) -> 
  string -> string ->
  ('b -> string) -> 
  ('c -> string) -> 
  'd
    (** [print m map_iter relation_delim elt_delim key_to_string data_to_string] prints to
    standard output the contents of map [m]. Each key and its binding
    is place on their own line separated by a tab. *)

val print_ :
  ?oc:out_channel ->
  'a ->
  (('b -> 'c -> unit) -> 'a -> 'd) -> 
  string -> string ->
  ('b -> string) -> 
  ('c -> string) -> 
  'd
    (** Same as [print]. *)

val to_list :
  'a ->
  (('b -> 'c -> ('b * 'c) list -> ('b * 'c) list) -> 'a -> 'd list -> 'e) ->
  'e
  (** [to_list m map_fold] converts the map to a (key,data) list.*)




val to_file :
  string ->
  string ->
  string ->
  'a ->
  (('b -> 'c -> string -> string) -> 'a -> string -> string) ->
  ('b -> string) -> ('c -> string) -> unit
  (** [to_file filename relation_delim elt_delim m map_fold key_to_string
    data_to_string] writes the map to file [filename].  In this file, the [key] and
    its binding [data] are separated by [relation_delim] and each
    element in the map is separated by [elt_delim]. *)


val of_file :
    string ->
    string ->
    string ->
    ('a -> 'b -> 'c -> 'c) -> 'c -> (string -> 'a) -> (string -> 'b) -> 'c
    (** [of_file filename relation_delim elt_delim map_add map_empty
	key_of_string data_of_string] returns a map from file [filename]. 
    *)


