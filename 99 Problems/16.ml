let rec drop lst n acc =
  match lst with
  | [] -> List.rev acc  (* If the list is empty, return the accumulated list *)
  | h::t -> 
      if List.length acc = n 
      then List.rev acc @ t  (* If `acc` has `n` elements, stop and return the rest of the list *)
      else drop t n (h::acc)  (* Otherwise, keep accumulating elements *)

(* Example usage *)
