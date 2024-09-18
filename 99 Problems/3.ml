let rec k_th_element lst n acc = match lst with
  | h::t -> if List.length acc = n then h else k_th_element t n (h::acc)
