
let size m map_fold =
  map_fold (fun k d x -> x + 1) m 0


let to_string relation_delim elt_delim m map_fold key_to_string data_to_string = 
  map_fold
    (fun k d x -> x^elt_delim^((key_to_string k)^relation_delim^(data_to_string d)))
    m
    ""

let of_string s relation_delim elt_delim map_add map_empty key_of_string data_of_string = 
  let sl = Mstring.to_stringlist elt_delim s in
  List.fold_left (fun m s -> 
    let kd_list = Mstring.to_stringlist relation_delim s in
    if (List.length kd_list) <> 2 
    then raise (Failure("Mmap.of_string: Ill-defined Key-Data"))
    else   
      let key = (key_of_string (List.nth kd_list 0)) and
	  data = (data_of_string (List.nth kd_list 1)) in
      map_add key data m)
    map_empty
    sl

let print ?oc:(oc=stdout) m map_iter relation_delim elt_delim key_to_string data_to_string = 
  map_iter
    (fun k d -> output_string oc (((key_to_string k)^relation_delim^(data_to_string d))^elt_delim))
    m

let print_ = print

let to_list m map_fold = map_fold (fun k d l -> (k,d)::l) m []

let to_file filename relation_delim elt_delim m map_fold key_to_string data_to_string =
  File.of_string
    filename
    (to_string relation_delim elt_delim m map_fold key_to_string data_to_string)
    

let of_file filename relation_delim elt_delim map_add map_empty key_of_string data_of_string =
  of_string (File.to_string ~delim:"\n" filename)
    relation_delim elt_delim map_add map_empty key_of_string data_of_string
