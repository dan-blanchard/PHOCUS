(** 

    The Delim module contains a number of standard ways elements of abstract
    types can be written in a string.  Modules of DELIM_TYPE are arguments to
    functors of the other modules over abstract types to facilitate reading
    and writing from strings and file.

*)


(** Part of the input signature of {!Xset.Make} and {!Xlist.Make}. *)
module type DELIM_TYPE = 
sig 
  val delim : string (** The string used to delimit elements in string*)
  val lb : string  (** The string used to identify the beginning of the relevant string *)
  val rb: string  (** The string used to identify the end of the relevant string *)
end

(** Input signature of Delim.MakeTag. *)
module type TAG_TYPE = 
sig
  val tag : string 
end

(** MakeTag returns a module of DELIM_TYPE with [delim=""] and [lb =
    "<"^tag^">"] and [rb = "</"^tag^">"]. *)
module MakeTag (Tag : TAG_TYPE) : DELIM_TYPE


module None : 
sig 
  val delim : string   (** [delim = ""] *)
  val lb : string      (** [lb = ""]    *)
  val rb : string      (** [rb = ""]    *)
end

module No_space_wbs : 
sig 
  val delim : string   (** [delim = ""] *)
  val lb : string      (** [lb = "#"]    *)
  val rb : string      (** [rb = "#"]    *)
end

module Space_wb : 
sig 
  val delim : string  (** [delim = " "] *)
  val lb : string     (** [lb = "#"]    *)
  val rb : string     (** [rb = "#"]    *)
end

module Space_no_bs : 
sig 
  val delim : string  (** [delim = " "] *)
  val lb : string     (** [lb = ""]    *)
  val rb : string     (** [rb = ""]    *)
end

module No_space_abs : 
sig 
  val delim : string  (** [delim = ""] *)
  val lb : string     (** [lb = "<"]    *)
  val rb : string     (** [rb = ">"]    *)
end

module Point_abs : 
sig 
  val delim : string  (** [delim = "."] *)
  val lb : string     (** [lb = "<"]    *)
  val rb : string     (** [rb = ">"]    *)
end

module Newline_cbs : 
sig 
  val delim : string   (** [delim = "\n"] *)
  val lb : string      (** [lb = "{"]    *)
  val rb : string      (** [rb = "}"]    *)
end

module Newline_no_bs : 
sig 
  val delim : string   (** [delim = "\n"] *)
  val lb : string      (** [lb = ""]    *)
  val rb : string      (** [rb = ""]    *)
end

module Bar_sqbs : 
sig 
  val delim : string   (** [delim = "|"] *)
  val lb : string      (** [lb = "\["]    *)
  val rb : string      (** [rb = "\]"]    *)
end

module Bar_cbs : 
sig 
  val delim : string   (** [delim = "|"] *)
  val lb : string      (** [lb = "\{"]    *)
  val rb : string      (** [rb = "\}"]    *)
end

module Widebar : 
sig 
  val delim : string   (** [delim = " | "] *)
  val lb : string      (** [lb = ""]    *)
  val rb : string      (** [rb = ""]    *)
end


module Comma_sqbs : 
sig 
  val delim : string    (** [delim = ","] *)
  val lb : string       (** [lb = "\["]    *)
  val rb : string       (** [rb = "\]"]    *)
end

module Comma_no_bs : 
sig 
  val delim : string    (** [delim = ","] *)
  val lb : string       (** [lb = ""]    *)
  val rb : string       (** [rb = ""]    *)
end

module Comma_cbs : 
sig 
  val delim : string   (** [delim = ","] *)
  val lb : string      (** [lb = "\{"]    *)
  val rb : string      (** [rb = "\}"]    *)
end

module Comma_prs : 
sig 
  val delim : string   (** [delim = ","] *)
  val lb : string      (** [lb = "("]    *)
  val rb : string      (** [rb = ")"]    *)
end


module Semi_cbs : 
sig 
  val delim : string   (** [delim = ";"] *)
  val lb : string      (** [lb = "\{"]    *)
  val rb : string      (** [rb = "\}"]    *)
end

module Excl_no_bs : 
sig 
  val delim : string   (** [delim = "!"] *)
  val lb : string      (** [lb = ""]    *)
  val rb : string      (** [rb = ""]    *)
end

module Undr_no_bs :
sig
  val delim: string   (** [delim = "_"] *)
  val lb : string     (** [lb = ""]    *)
  val rb : string     (** [rb = ""]    *)
end

module Dash_no_bs :
sig
  val delim: string   (** [delim = "-"] *)
  val lb : string     (** [lb = ""]    *)
  val rb : string     (** [rb = ""]    *)
end

module Hash_no_bs :
sig
  val delim: string   (** [delim = "#"] *)
  val lb : string     (** [lb = ""]    *)
  val rb : string     (** [rb = ""]    *)
end

module Hash_cbs :
sig
  val delim: string   (** [delim = "#"] *)
  val lb : string     (** [lb = "{"]    *)
  val rb : string     (** [rb = "}"]    *)
end


module Colon_no_bs :
sig
  val delim: string   (** [delim = ":"] *)
  val lb : string     (** [lb = ""]    *)
  val rb : string     (** [rb = ""]    *)
end

module Mp : 
sig 
  val delim : string   (** [delim = "\t"] *)
  val lb : string      (** [lb = ""]    *)
  val rb : string      (** [rb = "\n"]    *)
end

