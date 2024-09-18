let split lst n = 
  let rec aux n acc = function
  | [] -> acc, []
  | h::t -> if n =0 then acc @ [t] else  aux (n-1) (h::acc)