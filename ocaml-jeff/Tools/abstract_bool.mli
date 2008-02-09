module type ABSTRACT_BOOL_TYPE =
sig
  val name: string
  val pair: bool -> bool -> bool
end

module Make (Core:ABSTRACT_BOOL_TYPE) : X.X_TYPE
