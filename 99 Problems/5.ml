let rec rev_lst lst = match lst with  
  | [] -> []
  | h::t -> h:: rev_lst t