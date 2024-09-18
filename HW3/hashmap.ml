(* let count_occurrences list1 =
  let rec count acc x = function
    | [] -> (x, acc)::[]
    | (y, n)::rest ->
        if x = y then (y, n+acc)::rest
        else if x > y then (x, acc)::(y, n)::rest
        else (x, acc)::(y, n)::rest
  in
  let compare (x1, _) (x2, _) = compare x2 x1 in
  List.sort compare (List.fold_left (fun acc x -> count 1 x acc) [] list1) *)



let encode list =
   let rec aux count acc = function
  | [] -> [] (* Can only be reached if original list is empty *)
  | [x] -> (count + 1, x) :: acc
  | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                          else aux 0 ((count + 1, a) :: acc) t in
  List.rev (aux 0 [] list);;