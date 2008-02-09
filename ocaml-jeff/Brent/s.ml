let name = "Segment"
type t = string
let compare = compare
let pair x y = raise (Failure("NonCombinable"))
let of_string x = x
let to_string x = x

let print ?oc:(oc=stdout) n = 
  output_string oc (to_string n);
  output_string oc "\n"

let print_ ?oc:(oc=stdout) n = 
  output_string oc (to_string n)
      
