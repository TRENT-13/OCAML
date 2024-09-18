let rec zip_2_fun a b acc = match (a, b) with
  | ([], _) | (_, []) -> List.rev acc  (* Return reversed accumulator if any of the lists is empty *)
  | (x::xs, y::ys) -> zip_2_fun xs ys ((x, y) :: acc);;
