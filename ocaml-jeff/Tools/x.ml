module type X_TYPE =
  sig
    type t
    val name: string
    val compare: t -> t -> int
    val pair: t -> t -> t
    val of_string: string -> t
    val to_string: t -> string 
    val print: ?oc:out_channel -> t -> unit
    val print_ : ?oc:out_channel -> t -> unit 
  end
