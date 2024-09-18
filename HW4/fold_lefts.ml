let f1 lst = List.rev (List.fold_left (fun acc (a1,x1) -> (x1,a1) :: acc) [])

let f2 lst = List.fold_left (fun  acc x -> if List.length acc mod 2 = 0 then x :: acc else  acc @ [x]) [] lst


let f3 lst  = List.fold_left (fun f (a,b) -> fun x -> if x=a then b else f a ) (fun a -> 1)







