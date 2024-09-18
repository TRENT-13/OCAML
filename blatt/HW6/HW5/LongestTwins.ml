let rec find_longest_repeated_seq lst =
  let rec find_longest_subseq acc curr = function
    | [] -> acc
    | x :: xs ->
      if List.mem x curr then
        let new_curr = x :: curr in
        find_longest_subseq 
        (if List.length new_curr > List.length acc then new_curr else acc) new_curr xs
      else
        find_longest_subseq acc curr xs
  in
  match lst with
  | [] -> []
  | x :: xs -> find_longest_subseq [] [x] xs

  

