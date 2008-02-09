module type ABSTRACT_BOOL_TYPE =

sig
  val name: string
  val pair: bool -> bool -> bool
end

module Make (Core:ABSTRACT_BOOL_TYPE) =

struct
  let name = Core.name
  type t = bool
  let compare = compare
  let pair = Core.pair
  let of_string s = 
    try bool_of_string s
    with _ -> raise (Failure("Value.of_string: Unrecognizable Value"))
	  
  let to_string = function
      true -> "true"
    | false -> "false"

  let print ?oc:(oc=stdout) x = 
    output_string oc (to_string x);
    output_string oc "\n";
    flush oc
      
  let print_ ?oc:(oc=stdout) x = 
    output_string oc (to_string x);
    flush oc
end
