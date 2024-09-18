type 'a tree = 
| Leaf
| Node of 'a * 'a tree * 'a tree

 

let rec find x t = match t with
Leaf -> false
| Node(y, left, right) -> if x < y then find x left
                          else if x > y then find x right
                          else true;;