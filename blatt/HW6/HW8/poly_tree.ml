type tree = Empty
          | Node of int * tree * tree;;


let rec insert v compare = function Empty -> Node (v, Empty, Empty)
  | Node (x, l, r) -> let c = compare v x in
    if c < 0 then Node (x, insert v compare l, r)
    else if c > 0 then Node (x, l, insert v compare r)
    else Node (x, l, r)

let rec string_of_tree v_to_string = function Empty -> "Empty"
  | Node (v, l, r) -> "Node (" ^ (v_to_string v) ^ ", " ^
    (string_of_tree v_to_string l) ^ ", " ^ (string_of_tree v_to_string r) ^ ")"

(*
let inorder_list t =
  let rec impl q t acc = match t with Node (x, l, r) ->  impl (Node (x, Empty, r)::q) l acc
    | Empty -> match q with [] -> acc | Node (x, _, r)::qs -> impl qs r (x::acc)
    | _ -> failwith "unreachable"
  in
  List.rev (impl [] t [])
*)

let rec inorder_list t = match t with Empty -> [] | Node (x, l, r) -> (inorder_list l) @ x::(inorder_list r)
