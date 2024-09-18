let rec add lst = function 
  | [] -> []
  | h :: t -> (h+1) :: add t
  
  
let rec concat lst = function 
  | [] -> []
  | h::t -> (h ^"TOP G") :: concat t 
  


let rec map f = function 
  | [] -> []
  | h::t -> f h :: map f t

let add1 lst = map (fun (x) -> x+1) lst
let concat1 lst = map (fun (x) -> x ^"top G") lst ;;


let strignlist_of_intlist lst = map string_of_int lst
