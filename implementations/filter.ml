let rec even = function
  | [] -> []
  | h ::t -> if h mod 2 = 0 then h :: even t else even t
  



let rec odd = function
 | [] -> []
 | h ::t -> if h mod 2 = 1 then h :: odd t else odd t   


let rec filter p = function 
  | [] -> []
  | h::t -> if p h then h :: filter p t else filter p t 
 

let even x = x mod 2 = 0
let odd x = x mod 2 = 1

let even' lst = filter even lst
let odd' lst = filter (fun  x-> x mod 2 = 1) lst 




let rec filter_tail_rec p acc = function
  | [] -> List.rev acc
  | h::t -> 
    filter_tail_rec p (if p h then h :: acc else acc) t