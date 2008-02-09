module type ABSTRACT_STRING_TYPE =

sig
  val name: string
  val pair: string -> string -> string
end

module Make (Core:ABSTRACT_STRING_TYPE) =

struct
  let name = Core.name
  type t = string
  let compare = compare
  let pair = Core.pair
  let of_string x = x
  let to_string x = x
    
  let print ?oc:(oc=stdout) x = 
    output_string oc (to_string x);
    output_string oc "\n";
    flush oc
      
  let print_ ?oc:(oc=stdout) x = 
    output_string oc (to_string x);
    flush oc
end
