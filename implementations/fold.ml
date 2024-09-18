let rec sum =  function
| [] -> 0
| h:: t -> h+ sum t


let rec concat = function
| [] -> ""
| h::t -> h ^ concat t 



let rec combine acc  op = function
| [] -> acc
| h :: t -> op h (combine acc op  t)


let sum' lst = combine 0 (+) ;;

let concat lst = combine "" (^);;