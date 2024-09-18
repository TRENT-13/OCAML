let rec all_but_last = function
  | [] -> []
  | [_] -> []
  | h :: t -> h :: all_but_last t



  (* official site option *)
  let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;