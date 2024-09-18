type tree = Empty
          | Node of int * tree * tree;;


let rec insert x t = match t with
  Empty -> Node(x, Empty, Empty)
  | Node(y, left, right) -> if x < y then Node(y, insert x left, right)
                            else if x > y then Node(y, left, insert x right)
                            else t;;





let rec find x t = match t with
Empty -> false
| Node(y, left, right) -> if x < y then find x left
                          else if x > y then find x right
                          else true;;

let rec findmax t = match t with
  Empty -> raise Not_found
  | Node(x, _, Empty) -> x
  | Node(_, _, right) -> findmax right;;

let rec findmin t = match t with
  Empty -> raise Not_found
  | Node(x, Empty, _) -> x
  | Node(_, left, _) -> findmin left;;


let rec delete x t = match t with
  Empty -> Empty
  | Node(y, left, right) -> if x < y then Node(y, delete x left, right)
                            else if x > y then Node(y, left, delete x right)
                            else if left = Empty then right
                            else if right = Empty then left
                            else let m = findmax left in
                                 Node(m, delete m left, right);;

                                 
let rec tolist t = match t with
 | Empty -> []



 let rec in_order_trav t =
  let rec aux acc = function
    | Empty -> acc
    | Node(v, l, r) -> aux (v :: aux acc r) l
  in
  aux [] t