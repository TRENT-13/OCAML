module Sort = struct
  let single lst  = List.map (fun x  -> [x]) lst 
  let rec merge l1 l2 = match (l1,l2) with
  | [], [] -> [] 
  | x::xs, [] -> x::xs
  | [], y::ys -> y:: ys 
  | x::xs, y::ys -> if x < y then x :: merge xs l2 else y :: merge l1 ys


  let rec merge_lsts = function
  | [] -> []
  | [l] -> [l]
  | l1::l2::l3 -> merge l1 l2 :: merge_lsts l3
  
  let sort lst = 
    let lst = single lst 
  in let rec doit = function
  | [] -> [] | [l] -> [l]
  | l -> doit (merge_lsts l) in doit lst
end

module type Sort = sig
  val merge : 'a list -> 'a list -> 'a list
  val sort : 'a list -> 'a list
end
  