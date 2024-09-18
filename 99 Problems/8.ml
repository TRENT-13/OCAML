let rec compress lst acc  = match lst with
  | [] -> [] 
  | h::t -> if List.hd acc = h then compress t acc else  compress t (h::acc)