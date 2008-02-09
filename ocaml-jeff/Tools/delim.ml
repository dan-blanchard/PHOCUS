module type DELIM_TYPE = sig val delim : string val lb : string val rb: string end

(*  wbs : word boundary braces # *)
(*  abs : angles braces <> *)
(*  cbs : curly braces {} *)
(*  sqbs : square braces # *)
(*  no_bs : no braces # *)
(*  wbs : word boundary braces # *)
(*  wbs : word boundary braces # *)

module type TAG_TYPE = sig val tag : string end

module MakeTag (Tag : TAG_TYPE) =
struct
  let delim = ""
  let lb = "<"^(Tag.tag)^">"
  let rb = "<\\"^(Tag.tag)^">"
end

module None =          struct let delim = "" 	let lb = ""    let rb = ""   end
module No_space_wbs =  struct let delim = ""    let lb = "#"   let rb = "#"  end
module Space_wb =      struct let delim = " "   let lb = "#"   let rb = "#"  end
module Space_no_bs =   struct let delim = " "   let lb = ""    let rb = ""   end
module No_space_abs =  struct let delim = ""    let lb = "<"   let rb = ">"  end
module Point_abs =     struct let delim = "."   let lb = "<"   let rb = ">"  end
module Newline_cbs =   struct let delim = "\n"  let lb = "{"   let rb = "}"  end
module Newline_no_bs = struct let delim = "\n"  let lb = ""    let rb = ""   end
module Bar_sqbs =      struct let delim = "|"   let lb = "["   let rb = "]"  end
module Bar_cbs =       struct let delim = "|"   let lb = "{"   let rb = "}"  end
module Widebar =       struct let delim = " | " let lb = ""    let rb = ""   end
module Comma_sqbs =    struct let delim = ","   let lb = "["   let rb = "]"  end
module Comma_no_bs =   struct let delim = ","   let lb = ""    let rb = ""   end
module Comma_cbs =     struct let delim = ","   let lb = "{"   let rb = "}"  end
module Comma_prs =     struct let delim = ","   let lb = "("   let rb = ")"  end
module Semi_cbs =      struct let delim = ";"   let lb = "{"   let rb = "}"  end
module Excl_no_bs =    struct let delim = "!"   let lb = ""    let rb = ""   end
module Undr_no_bs =    struct let delim = "_"   let lb = ""    let rb = ""   end
module Dash_no_bs =    struct let delim = "-"   let lb = ""    let rb = ""   end
module Hash_no_bs =    struct let delim = "#"   let lb = ""    let rb = ""   end
module Hash_cbs =    struct let delim = "#"   let lb = "{"    let rb = "}"   end
module Colon_no_bs =    struct let delim = ":"   let lb = ""    let rb = ""   end

module Mp =         struct let delim = "\t"   let lb = ""    let rb = "\n" end
