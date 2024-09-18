type 'a one_two_tree =
|Empty
|OneNode of 'a * 'a one_two_tree
|TwoNode of 'a * 'a one_two_tree * 'a one_two_tree


let rec extract_min t = function
| Empty  -> None, Null
| OneNode (a,t) -> Some a, t
| TwoNode (l,a,r) -> match extract_min l with
  | None, Null -> Some a ,r
  | Some k,t -> Some k, TwoNode(t,a,r)
 

let rec inorder t = match t with
| Empty -> []
| OneNode (a,t) -> a :: inorder t
|TwoNode(l,a,r) -> inorder l @ ( a :: inorder r)


let verify 


let rec to_binary t = match t with
|Empty -> Empty
|OneNode (x, t) -> TwoNode (x, to_binary t, Empty)
|TwoNode (x, t1, t2) -> TwoNode (x, to_binary t1, to_binary t2);;


let rec remove_duplicates l = match l with
|[] -> []
|x::xs -> x::remove_duplicates (List.filter (fun y -> y <> x) xs);;


let rec from_list l = match l with
|[] -> Empty
|x::xs -> OneNode (x, from_list (remove_duplicates xs));;


