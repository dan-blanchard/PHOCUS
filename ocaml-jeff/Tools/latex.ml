(* 

   This file contains functions for reading and writing to files
   Last Updated: May 26,2005
  
*)


let preamble title = 
  ( 
    "\\documentclass[11pt]{article}\n\n"^

      "\\usepackage{natbib}\n"^
      "\\bibpunct{(}{)}{,}{a}{}{,}\n\n"^
      
      "\\usepackage{vmargin}\n"^
      "\\setpapersize{USletter}\n"^
      "\\setmarginsrb{1in}{1in}{1in}{1in}{0pt}{0mm}{0pt}{0mm}\n\n"^

      "\\usepackage{multicol}\n"^
      "\\usepackage{amssymb}\n"^
      "\\usepackage{epstopdf}\n"^
      "\\usepackage{pstricks}\n"^
      "\\usepackage{graphicx}\n"^
      "\\usepackage{colortab}\n"^
      "\\usepackage{arydshln}\n"^
      "\\usepackage{pifont}\n"^
      "\\usepackage[safe]{tipa}\n"^
      "\\usepackage{linguex}\n\n"^

      "\\title{"^title^"}\n"^
      "\\begin{document}\n\n"^
      "\\bibliographystyle{ma}\n\n"^
      "\\maketitle\n\n"
  )

let figure caption figure = 
  ( 
    "\\begin{figure}\n"^
      "\\begin{center}\n"^
      figure^
      "\\end{center}\n"^
      "\\caption{"^caption^"}\n"^
      "\\end{figure}\n\n"
  ) 


let btabular pos cols = 
  ( 
    "\\begin{tabular}\n"^
      "["^pos^"]{"^cols^"}\n"
  ) 

let etabular = "\\end{tabular}\n\n"
  
let tabular pos cols rows = 
  (btabular pos cols)^rows^etabular

let graphics file = ("\\includegraphics{"^file^"}\n")

let close = ("\\end{document}")


let compile file = 
  (* compile runs the following commands *)
  let filename = File.name_rm_ext file in
  let _ = Unix.system ("latex "^filename^".tex") in
  ()
    
let launch file = 
  let filename = File.name_rm_ext file in
  let _ = compile file in
  Unix.system ("open "^filename^".ps");;

