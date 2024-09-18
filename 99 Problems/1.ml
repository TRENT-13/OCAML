let rec last_el lst = match lst with
  | [] -> failwith"empty"
  | [a] -> a
  | _ :: t -> last_el t