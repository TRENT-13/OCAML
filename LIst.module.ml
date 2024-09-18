module Sisiis = struct
  let rec hd lst = function
  | [] -> []
  | h::_ ->  h 

  let rec tl lst = function
  | [] -> []
  | _ :: t -> tl t

  let rec len lst = match  lst with
  | [] -> 0
  | h::t -> 1 + len t


  let rec app lst = function
  | [] -> []
  | h::t -> h @ app t
  
  let rec nth lst n = function
    | [] -> []
    | h::t -> if n = 0 then h else nth t (n-1)
end
