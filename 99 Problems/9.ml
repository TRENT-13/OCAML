let rec pack lst = 
  let rec aux curr acc = function
  | [] -> []
  | [x] -> (x::curr) ::acc
  | a ::(b :: _ as t) -> if a = b then aux (a::curr) acc t else aux [] ((a::curr) ::acc) t
in aux [] [] 

