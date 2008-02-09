module type ABSTRACT_STRING_TYPE =
sig
  val name: string
  val pair: string -> string -> string
end

module Make (Core:ABSTRACT_STRING_TYPE) : X.X_TYPE
