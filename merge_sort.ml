let rec split list = 
  match list with
  | [] -> ([], []) 
  | [a] -> (list, [])
  | x::x'::xs ->
    let left, right = split xs 
  in (x::left, x'::right)
;;



let rec merge l1 l2 = 
  match l1, l2 with
  | ([], []) -> []
  | (_, []) -> l1
  | ([], _) -> l2
  | (x::xs, y::ys) ->
    if x <= y then x::(merge xs l2)
    else y::(merge l1 ys)
;;

let rec msort list = 
  match list with
  | [] -> list 
  | [a] -> list
  | _ ->
    let (left, right) = split list in
    let left' = msort left in
    let right' = msort right in
    merge left' right'
;;