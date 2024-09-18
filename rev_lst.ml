  let rec app = function []-> fun y-> y
  | x::xs-> fun y-> x::app xs y;;


  let rec rev list = match list
 with []-> []
 | x::xs-> app (rev xs) [x]


 let rev list =
  let rec r2 a l =
  match l
  with []-> a
  | x::xs-> r2 (x::a) xs
  in r2 [] list

  
  let rec f = fun l->
    match l with []-> 1 | x::xs-> x + g xs
    and g = fun l->
    match l with []-> 0 | x::xs-> x * f xs