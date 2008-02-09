(* Example Usage of mbool.ml  *)

let p = (V('p'));;
let q = (V('q'));;
let r = (V('r'));;
let s = (V('s'));;
let t = (V('t'));;

let pq = [p;q];;
let pqr = [p;q;r];;
let pqrs = [p;q;r;s];;
let pqrst = [p;q;r;s;t];;

let pqp = pred_of_varlist pq;;
let pqrp = pred_of_varlist pqr;;
let pqrsp = pred_of_varlist pqrs;;
let pqrstp = pred_of_varlist pqrst;;

let f2l = make_functions pq NOLIM;;    (* making all boolean functions *)
let f6 = List.assoc 6 f2l;;            (* of 2 variables and looking   *)
truthtable pqp f6;;                    (* at the biconditional.        *)
let eps_of_f6 = expPS pq f6;;
print_ps eps_of_f6;;
truthtable pqp (funct_of_ps eps_of_f6);;

let ff2l = snd (List.split f2l);;         (* Viewing all the expanded *) 
let eps_list = List.map (expPS pq) ff2l;; (* power series sets of     *)
List.map (print_ps) eps_list;;            (* boolean functions of 2 vars *)

let f5l = make_functions pqrst (Lim(100));;    (* making the first 100 boolean *)
let f95 = List.assoc 95 f5l;;                  (* functions of 2 variables and *)
truthtable pqrstp f95;;                          (* looking the biconditional.   *)
let eps_of_f95 = expPS pqrst f95;;
print_ps eps_of_f95;;
truthtable pqrstp (funct_of_ps eps_of_f95);;

let num = List.length pqrst;;                  (* Here I just make some *)
let domain = get_args [(tuplemaker num)];;     (* big function of 5 vars *)
let range =                                    (* and check out the *) 
[                                              (* expanded power series *)
false;false;false;true;false;true;true;false;
false;false;false;true;true;false;false;true;
false;false;false;true;false;true;true;false;
false;false;false;true;true;false;false;true
];;

let f = build_fun pqrst range domain;;
truthtable pqrstp f;;                     
let eps_of_f = expPS pqrst f;;
print_ps eps_of_f;;
truthtable pqrstp (funct_of_ps eps_of_f);;
