(** Mbool offers several functions for constructing,viewing, and altering the
    forms of boolean predicates. Commenting is not complete, please see the
    mbool_examples.ml file.

    author : Jeff Heinz
    last updated : April 2005
*)

(** The negation operator. *)
type op1 = NOT

(** The binary operators over boolean predicates *)
type op2 = AND | OR | COND | IFF

(** The [variable] type *)
type variable = V of char

(** Boolean predictes *)
type predicate =
    P0 of variable
  | P1 of op1 * predicate
  | P2 of op2 * predicate * predicate
  | ZERO
  | ONE

(** Returns the value of a variable as a predicate. *)
val pos : variable -> predicate

(** Returns the opposite value of a variable as a predicate.*)
val minus : variable -> predicate

(** Returns a predicate list from a list of variables. *)
val pred_of_varlist : variable list -> predicate list

(** Returns the negation of a predicate. *)
val neg : predicate -> predicate

(** Returns the disjunction of two predicates. *)
val disj : predicate -> predicate -> predicate

(** Returns the conjunction of two predicates. *)
val conj : predicate -> predicate -> predicate

(** Returns the biconditional of two predicates. *)
val iff : predicate -> predicate -> predicate

(** Returns the conditional of two predicates. *)
val cond : predicate -> predicate -> predicate

(** Prints a predicate to standard output. *)
val prettyprint : predicate -> unit

val zip : 'a list -> 'b list -> ('a * 'b) list

(** Returns the truth value of a predicate given a list of predicates paired
    with their truth values. *)
val eval : predicate -> (predicate * bool) list -> bool

exception Finished
val next_tuple : bool list -> bool list
val tuplemaker : int -> bool list
val get_args : bool list list -> bool list list
val get_args_upto : int -> bool list list -> bool list list

val string_of_bool : bool -> string
val string_of_variables : predicate list -> string
val string_of_values : bool list -> string
val string_of_truthtable : predicate -> predicate list -> string
val truthtable : predicate list -> predicate -> unit
exception NoSubExpression

(** Returns a predicate in conjunctive normal form *)
val cnf : predicate -> predicate

(** Returns a predicate in disjunctive normal form *)
val dnf : predicate -> predicate
val expand : (predicate -> predicate) -> predicate -> predicate
val entails : predicate -> predicate -> predicate list -> bool

type literal = L0 of variable | L1 of op1 * variable
val poslit : variable -> literal
val neglit : variable -> literal
val literals_of_vl : variable list -> literal list
val opp : literal -> literal
module LiteralOrder : sig type t = literal val compare : t -> t -> int end
module LiteralSet :
  sig
    type elt = LiteralOrder.t
    type t = Set.Make(LiteralOrder).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module PowerOrder : sig type t = LiteralSet.t val compare : t -> t -> int end
module PowerSeriesSet :
  sig
    type elt = PowerOrder.t
    type t = Set.Make(PowerOrder).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val powerset_of_literalsetList : PowerSeriesSet.elt list -> PowerSeriesSet.t
val pred_of_l : literal -> predicate
val string_of_literal : literal -> string
val concat : string -> string -> string
val string_of_ls : LiteralSet.t -> string
val string_of_ps : PowerSeriesSet.t -> string
val print_ps : PowerSeriesSet.t -> unit
val funct_of_ls : LiteralSet.t -> predicate
val funct_of_ps : PowerSeriesSet.t -> predicate
val f_of_tt : (int list * int) list -> variable list -> predicate
exception NonCombinable
val add_l_to_literalset : LiteralSet.elt -> LiteralSet.t -> LiteralSet.t
val cross : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val cross_ps_ll : PowerSeriesSet.t -> LiteralSet.elt list -> PowerSeriesSet.t
val zeroPS_of_variables : variable list -> PowerSeriesSet.t
val phi_zero_f : predicate -> variable list -> PowerSeriesSet.t
val phibasic2 : int -> variable list -> PowerSeriesSet.t
val phibasic : int -> variable list -> PowerSeriesSet.t list
val conj_of_psList : PowerSeriesSet.t list -> predicate
val remover : PowerSeriesSet.t list -> variable list -> PowerSeriesSet.t
val remove_earlier_entailments :
  PowerSeriesSet.t list -> variable list -> PowerSeriesSet.t list
val phimaker : int -> predicate -> variable list -> PowerSeriesSet.t list
val phi : int -> predicate -> variable list -> PowerSeriesSet.t
val expPS : variable list -> predicate -> PowerSeriesSet.t
val conjunction : bool list -> variable list -> predicate
type limit = NOLIM | Lim of int
val build_fun : variable list -> bool list -> bool list list -> predicate
val make_functions : variable list -> limit -> (int * predicate) list
val print_fl : ('a * predicate) list -> variable list -> unit list
