 (*                                  *)
 (*   Jeff Heinz                     *)
 (*   Comp Ling 2 / Jan-Mar 2005     *)
 (*                                  *)
 (*   line    topic                  *)
 (*    17       typing               *)
 (*    42       print & eval functs  *)
 (*    91       making boolean lists *)
 (*   124       string functions     *)
 (*   178       CNF & DNF            *)
 (*   252       checking entailment  *)
 (*   280       typing of literals,  *)
 (*              literal sets, etc   *)
 (*   405       expanded ps funct    *)
 (*              beginning           *)
 (*   511       PHI function!        *)
 (*   522       ExpPS function!      *) 
 (*   529       Function Making      *) 
 (*   576       Examples             *)

type op1 = NOT

type op2 = AND | OR | COND | IFF

type variable = V of char

type predicate =   P0 of variable 
		 | P1 of op1 * predicate 
		 | P2 of op2 * predicate * predicate
		 | ZERO
		 | ONE

let pos x = P0(x)
let minus x = P1(NOT, P0(x))

let pred_of_varlist vl = 
  List.map (fun x -> P0(x)) vl 

let neg x = P1(NOT, x)
let disj x y = P2(OR,x,y) 
let conj x y = P2(AND,x,y)
let iff x y = P2(IFF,x,y)
let cond x y = P2(COND,x,y)


let prettyprint f =      (* PRINTING AND EVALUATING BOOLEAN FUNCTIONS *)
  let rec pp f =
    match f with 
      | ZERO -> "0"
      | ONE -> "1"
      | P2(IFF,g,h) -> "("^(pp g)^" <--> "^(pp h)^")"
      | P2(COND,g,h) -> "("^(pp g)^" --> "^(pp h)^")"
      | P2(AND,g,h) -> "("^(pp g)^" && "^(pp h)^")"
      | P2(OR,g,h) ->  "("^(pp g)^" || "^(pp h)^")"
      | P1(NOT,g) -> "-"^(pp g) 
      | P0(V(x)) -> Char.escaped x
  in 
  print_string ((pp f)^"\n\n")



let zip l v =
  let rec zhelper a b c = 
  match (a,b) with 
    | ([],[]) -> c
    | (h::t,x::y) -> zhelper t y ((h,x)::c)
    | _ -> failwith "Lists of Unequal size."
  in
  zhelper l v []



let rec eval f a =
  try
    match f with 
      | ZERO -> false
      | ONE -> true
      | P2(IFF,g,h) -> (eval (cond g h) a) && (eval (cond h g) a)
      | P2(COND,g,h) -> (eval (neg g) a) || (eval h a)
      | P2(AND,g,h) -> (eval g a) && (eval h a) 
      | P2(OR,g,h) -> (eval g a) || (eval h a) 
      | P1(NOT,g) -> not (eval g a)  
      | P0(x) -> List.assoc (P0(x)) a
  with
      _ -> failwith "The values of the arguments are not defined."


(*                      *)
(*  The functions below *)
(* lead up to printing  *)
(* a truthtable for any *)
(* boolean function.    *)
(*                      *)

exception Finished           (* MAKING BOOLEAN LISTS        *)

let rec next_tuple orig =
  match orig with 
    | [] -> raise Finished
    | true::t -> false::(next_tuple t)
    | false::t -> (true::t) 

let tuplemaker num =
  let rec helper n l =
    match n with 
      | 0 -> l
      | m -> helper (m-1) (false::l)
  in
  helper num [] 
 
let rec get_args arglist = 
    try
      let newarg= (next_tuple (List.hd arglist)) in
      get_args (newarg::arglist) 
    with 
	Finished -> arglist

let rec get_args_upto k arglist = 
    try
      let size = List.length arglist in
      if k = size + 1 then raise Finished
	else 
	  let newarg= (next_tuple (List.hd arglist)) in
	  get_args_upto k (newarg::arglist) 
    with 
	Finished -> arglist

let string_of_bool b =               (* STRING FUNCTIONS BEGIN HERE *)
  match b with
    | true -> "1"
    | false -> "0"

let string_of_variables variablelist =    
  let rec row_of_string ll =
  match ll with
    | [] -> ""
    | (P0(V(x)))::t -> "| "^(Char.escaped x)^" "^(row_of_string t)
    | _ -> failwith "Not a variable"
  in 
  row_of_string variablelist


let string_of_values valuelist =
  let rec row_of_string vl =
  match vl with
    | [] -> ""
    | h::t -> "| "^(string_of_bool h)^" "^(row_of_string t)
  in 
  row_of_string valuelist


let string_of_truthtable f l =
  let rec helper al =
    match al with
      | [] -> ""
      | h::t -> 
	  let v = zip l h in	  
	  (string_of_values h)^
	    "|| "^(string_of_bool (eval f v))^" |\n"^
          (helper t)
  in
  let card_l = List.length l in
  let vector = tuplemaker card_l in
  let arglist = get_args [vector] in
  let rec make_line n =
    match n with
      | 0 -> ""
      | a -> "+---"^(make_line (a-1))
  in
  let line = (make_line card_l)^"++---+\n" in
  "\n"^(string_of_variables l)^"|| f |\n"^(line)^
  (helper arglist)^"\n"

let truthtable l f=
  let tt = string_of_truthtable f l in
  let () = print_string tt in
  let () =   print_string ("where f = ") in
  prettyprint f



(*                      *)
(*  The functions below *)
(* convert any boolean  *)
(* function to CNF or   *)
(* DNF. (but it doesn't *)
(* simplify.)           *)
(*                      *)

exception NoSubExpression

let cnf f =
    match f with 
      | P2(IFF,g,h) -> (conj (cond g h) (cond h g))
      | P2(COND,g,h) -> (disj (neg g) h) 

      | P1(NOT, P2(OR,g,h)) -> (conj (neg g) (neg h))
      | P1(NOT, P2(AND,g,h)) -> (disj (neg g) (neg h))
      | P1(NOT, P1(NOT,x)) -> x

      | P2(OR,g, P2(AND,h,j)) -> (conj (disj g h) (disj g j))
      | P2(OR,P2(AND,h,j),g) -> (conj (disj h g) (disj j g))
 
      | _ -> raise NoSubExpression

let dnf f =
    match f with 
      | P2(IFF,g,h) -> (conj (cond g h) (cond h g))
      | P2(COND,g,h) -> (disj (neg g) h) 

      | P1(NOT, P2(OR,g,h)) -> (conj (neg g) (neg h))
      | P1(NOT, P2(AND,g,h)) -> (disj (neg g) (neg h))
      | P1(NOT, P1(NOT,x)) -> x

      | P2(AND,g, P2(OR,h,j)) -> (disj (conj g h) (conj g j))
      | P2(AND, P2(OR,h,j),g) -> (disj (conj h g) (conj j g)) 
      
      | _ -> raise NoSubExpression


let rec expand exp f = 
  try 
    let g = 
      match f with 
	| P2(op,x,y) -> 
 	    (
	      try exp f
	      with 
	 	  NoSubExpression ->
 		    (
		      try (P2(op, exp x, y)) 
  		      with
  	 	 	  NoSubExpression -> 
	  		    (
	   		      try (P2(op, x, exp y))
     			      with
		 	 	  NoSubExpression -> raise NoSubExpression
  	 		    ) 
    		    )
   	    )
	| P1(op,x) -> 
 	    (
 	      try (P1(op, exp x)) 
	      with NoSubExpression -> raise NoSubExpression
	    ) 
  	| P0(_) 
	| ZERO
	| ONE -> raise NoSubExpression
	
    in
    expand exp g
  with 
      NoSubExpression -> f


let entails f g l =        (* CHECKING ENTAILMENT FUNCTION *)
  let rec helper al =
    match al with
      | [] -> true
      | h::t -> 
	  let vals = zip l h in
	  if (eval f vals) then 
	    if (eval g vals) then
	      helper t
	    else
	      false	      
	  else 
	    helper t
  in	    
  let card_l = List.length l in
  let vector = tuplemaker card_l in
  let arglist = get_args [vector] in
  helper arglist


(*                      *) 
(*  The typing below    *) 
(* establishes literals,*) 
(* literal sets, and    *) 
(* sets of literal sets *) 
(* which I call power   *) 
(* series sets.         *) 
(*                      *) 

type literal = L0 of variable | L1 of op1 * variable

let poslit v = L0(v)
let neglit v = L1(NOT,v)

let literals_of_vl variablelist =
  let rec helper vl ll =
    match vl with
      | [] -> ll
      | h::t -> helper t (poslit h::(neglit h::ll))
  in
  helper variablelist []

let opp l = 
  match l with 
    | L0(v) -> L1(NOT,v)
    | L1(NOT,v) -> L0(v)


module LiteralOrder : Set.OrderedType with type t = literal = 
struct 
  type t = literal
  let compare x y = 
    match (x,y) with
      | (L1(_,V(a)),L0(V(b))) when a=b -> -1
      | (L0(V(a)),L1(_,V(b))) when a=b -> 1
      | (L0(V(a)),L0(V(b))) 
      | (L1(_,V(a)),L1(_,V(b)))
      | (L1(_,V(a)),L0(V(b))) 
      | (L0(V(a)),L1(_,V(b))) -> Char.compare a b

end 
module LiteralSet = Set.Make(LiteralOrder) 

module PowerOrder : Set.OrderedType with type t = LiteralSet.t =
struct 
  type t = LiteralSet.t
  let compare x y =
    LiteralSet.compare x y
end
module PowerSeriesSet = Set.Make(PowerOrder)

let powerset_of_literalsetList lslist =
  let rec helper lslist set =
    match lslist with
      | [] -> set
      | h::t -> helper t (PowerSeriesSet.add h set)
  in
  helper lslist PowerSeriesSet.empty

let pred_of_l l = 
  match l with
    | L0(x) -> P0(x)
    | L1(NOT,x) -> neg (P0(x))

let string_of_literal l =
  match l with
    | L0(V(x)) -> Char.escaped x
    | L1(NOT,V(x)) -> "-"^(Char.escaped x)

let concat x y = x^y

let string_of_ls ls =                   (* STRING FUNCTIONS *)
  match LiteralSet.cardinal ls with
    | 0 -> "{}"
    | 1 -> "-{"^(string_of_literal (LiteralSet.choose ls))^"} "
    | _ ->   
	let lset = LiteralSet.fold 
	  (fun x -> concat (string_of_literal x)) ls "" in
        "-{"^lset^"} "

let string_of_ps ps =
  match PowerSeriesSet.cardinal ps with
    | 0 -> "\n{}\n\n"
    | 1 -> string_of_ls (PowerSeriesSet.choose ps)
    | _ ->   
	let pset = PowerSeriesSet.fold 
	  (fun x -> concat (string_of_ls x)) ps "" in
        ("\n{"^pset^"}\n\n")

let print_ps ps = print_string (string_of_ps ps)

let funct_of_ls ls =                   (*FUNCTIONS OF SETS*)
  match LiteralSet.cardinal ls with
    | 0 -> failwith "Empty Literal Set"
    | 1 -> neg (pred_of_l (LiteralSet.choose ls))
    | _ ->   
	let first = LiteralSet.choose ls in
	let newls = LiteralSet.remove first ls in
	let conjunction = LiteralSet.fold 
	  (fun x -> conj (pred_of_l x)) newls (pred_of_l first) in
        neg conjunction

let funct_of_ps ps = 
  match PowerSeriesSet.cardinal ps with
    | 0 -> ONE
    | 1 -> funct_of_ls (PowerSeriesSet.choose ps)
    | _ -> 	
	let first = PowerSeriesSet.choose ps in
	let newps = PowerSeriesSet.remove first ps in
	let conjunction = PowerSeriesSet.fold 
	  (fun x -> conj (funct_of_ls x)) newps (funct_of_ls first) in
        conjunction


let rec f_of_tt ll vs =   
  let rec row_conj r vl = 
    match (r,vl) with
      | (0::[], v::[]) -> minus v 
      | (_::[], v::[]) -> pos v
      | (0::t, v::vt) -> conj (minus v) (row_conj t vt)
      | (_::t, v::vt) -> conj (pos v) (row_conj t vt)
      | _ -> failwith "Number of variables does not match truth table vector size." 
  in
  match ll with 
    | [] -> ZERO
    | (r,0)::t -> (f_of_tt t vs)
    | (r,_)::[] -> row_conj r vs
    | (r,_)::t -> disj (row_conj r vs) (f_of_tt t vs)


(* SETTING UP TO GET THE EXPANDED *)
(* POWER SERIES SET               *)

exception NonCombinable

let add_l_to_literalset l ls =
  if LiteralSet.mem l ls || LiteralSet.mem (opp l) ls then raise NonCombinable
  else LiteralSet.add l ls

let cross pair s1 s2 =
  let rec crossHelper x s1 s2 = match s1 with 
    |[] -> s2 
    |(hd::tl) -> try (crossHelper x tl ((pair x hd)::s2)) with
	| NonCombinable -> crossHelper x tl s2
  in  
  let rec crossHelperDos s1 s2 s3 = match s1 with 
    |[] -> s3 
    |(hd::tl) -> crossHelperDos tl s2 (crossHelper hd s2 s3) 
  in 
  crossHelperDos s1 s2 [] 

let cross_ps_ll ps ll =                     (*crosses a ps with a literallist*)
  let ps_list = PowerSeriesSet.elements ps in
  let litset_list = cross (add_l_to_literalset) ll ps_list in
  powerset_of_literalsetList litset_list

let zeroPS_of_variables variablelist =  (* the zero function ps *)
  let rec helper ll set =
    match ll with
      | [] -> set
      | h::t -> 
	  helper t (PowerSeriesSet.add (LiteralSet.singleton h) set)
  in 
  let lits = literals_of_vl variablelist in
  helper lits PowerSeriesSet.empty

let phi_zero_f f vl =
  let ps = zeroPS_of_variables vl in
  let predlist = List.map (fun x -> pos x) vl in   (* the entails
						      function uses
						      predicates *)
  PowerSeriesSet.filter (fun x -> entails f (funct_of_ls x) predlist) ps

let phibasic2 n vl =     (*UNUSED*)
  let lits = literals_of_vl vl in
  let rec helper i powerset =
    match i with
      | 0 -> zeroPS_of_variables vl
      | _ -> cross_ps_ll (helper (i-1) powerset) lits 
  in 
  helper n PowerSeriesSet.empty

let phibasic n vl =
  let literals = literals_of_vl vl in
  let rec helper i list =
    if i == 0 then 
      helper (i+1) [zeroPS_of_variables vl]
    else 
      if i == (n+1) then
	list 
      else 
	helper (i+1) ((cross_ps_ll (List.hd list) literals)::list) 
  in 
  helper 0 []

let rec conj_of_psList pslist =
    match pslist with
      |	h::[] -> funct_of_ps h 
      | h::t -> conj (funct_of_ps h) (conj_of_psList t)
      | _ -> failwith "Empty List" 

let remover pslist vl =          (* We only want to use this function *)
  let head = List.hd pslist in   (* if we know that the tail is       *)
  let tail = List.tl pslist in   (* well-formed, i.e. is power exp    *)
  if tail = [] then head
  else 
    let predlist = List.map (fun x -> pos x) vl in   (* the entails
	  						function uses
						      predicates *)
    PowerSeriesSet.filter 
      (fun x -> 
	not (entails (conj_of_psList tail) (funct_of_ls x) predlist))
      head

let remove_earlier_entailments pslist vl = 
  let rec helper psl conjl= 
    match psl with
      | [] -> []
      | h::t -> 
	  let newconjl = h::conjl in
	  (remover newconjl vl)::(helper t newconjl)
  in
  helper (List.rev pslist) []

let phimaker n f vl =   
  let predlist = List.map (fun x -> pos x) vl in   (* the entails
	  					      function uses
		  				      predicates *)
  let pslist = phibasic n vl in
  let pslist2 = List.map                 (* removes from each ps the *)
    (fun y ->                            (* literal sets entailed by f *)
      PowerSeriesSet.filter 
      (fun x -> entails f (funct_of_ls x) predlist) y) pslist
  in
  let pslist3 = remove_earlier_entailments pslist2 vl in
  pslist3

let phi n f vl =           (* The PHI function in the notes! *)      
  let lits = List.length (literals_of_vl vl) in
  if n > lits 
  then  
    failwith "Phi is undefined for this n"
  else 
    let pslist = phimaker n f vl in
    let powerSetExp = List.hd (List.rev pslist) in
    let () = print_ps powerSetExp in
    powerSetExp

let expPS vl f=    (* The expanded power series function in the notes! *)
  let num = (List.length vl) -1 in
  let pslist = phimaker num f vl in
  List.fold_left (PowerSeriesSet.union) PowerSeriesSet.empty pslist

(* Making all the functions for a variable list *)

let rec conjunction boolList vl =
  match (boolList,vl) with 
    | (true::[],hv::[]) -> pos hv
    | (false::[],hv::[]) -> minus hv
    | (true::tb,hv::tv) -> conj (pos hv) (conjunction tb tv)
    | (false::tb,hv::tv) -> conj (minus hv) (conjunction tb tv)
    | ([],[]) -> failwith "this should not be happening"
    | _ -> failwith "The numbner of variables does not match the number of values "
  

type limit = NOLIM | Lim of int  (* this is so we dont run out of *)
                                   (* space getting all functions of *)
                                   (* 4 variables!                   *)

let rec build_fun vl range domain = 
  match (range, domain) with
    | (false::[],hd::[]) -> ZERO
    | (true::[],hd::[]) -> conjunction hd vl
    | (true::tr,hd::td) ->
        disj (conjunction hd vl) (build_fun vl tr td)
    | (false::tr,hd::td) -> build_fun vl tr td
    | _ -> failwith "Domain and Range don't match in size"

let make_functions vl limit = 
  let num = List.length vl in
  let domain = get_args [(tuplemaker num)] in
  let rangeSize = List.length domain in
  let zero_funct = tuplemaker rangeSize in
  let rangeValuesList = 
    match limit with 
      | Lim(k) -> get_args_upto k [zero_funct]
      | _ -> get_args [zero_funct]
  in
  let rec helper rvl flist n =   (* flist is indexed *)
    match rvl with
      |	[] -> flist
      | h::t -> helper t ((n,(build_fun vl h domain))::flist) (n+1)
  in 
  helper rangeValuesList [] 0

let print_fl functionList vl= 
  let predList = pred_of_varlist vl in
  let fl = snd (List.split functionList) in
  List.map (truthtable predList) fl 

