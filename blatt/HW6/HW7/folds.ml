let length lst = List.fold_left (fun acc x -> acc+1) 0 lst


let longest_list lists =
  match List.fold_left (fun acc lst -> if List.length lst > List.length acc then lst else acc) [] lists with
  | [] -> []
  | longest -> longest


let swap lst =  
  List.fold_left(fun acc (x1,x2) -> (x2,x1) :: acc) [] lst

let modulo lst = List.fold_left (fun  acc x -> if List.length acc mod 2 = 0 then x :: acc else  acc @ [x]) [] lst


let funqcia lst  = List.fold_left (fun f (a,b) -> fun x -> if x=a then b else f a ) (fun a -> 1)
  


let funqciebi lst = List.fold_left (fun (h::t) f-> f h ::(h::t)) []