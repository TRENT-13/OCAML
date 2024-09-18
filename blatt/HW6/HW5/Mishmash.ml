let rec interleave3 (list1: 'a list) (list2: 'a list) (list3: 'a list): 'a list =
  match (list1, list2, list3) with
  | [], [], [] -> [] (* All lists are empty, return empty list *)
  | [], ys, zs -> ys @ zs (* List1 is empty, append remaining elements *)
  | xs, [], zs -> xs @ zs (* List2 is empty, append remaining elements *)
  | xs, ys, [] -> xs @ ys (* List3 is empty, a5ppend remaining elements *)
  | x::xs1, y::ys1, z::zs1 -> x :: y :: z :: interleave3 xs1 ys1 zs1 



  (* let rec interleave3 (list1: 'a list) (list2: 'a list) (list3: 'a list): 'a list =
  match (list1, list2, list3) with
  | [], [], [] -> []
  | [], ys, zs -> fold_right (fun x acc -> x :: acc) ys zs
  | xs, [], zs -> fold_right (fun x acc -> x :: acc) xs zs
  | xs, ys, [] -> fold_right (fun x acc -> x :: acc) xs ys
  | x::xs1, y::ys1, z::zs1 ->
      x :: (interleave3 xs1 (y :: ys1) zs1) *)




let rec f a b c = match a with
  | [] -> if ((List.length b = 0) && (List.length c = 0)) then [] else f b c a
  | h :: t -> h :: f b c t;;