type 'a tree = Leaf of 'a 
  | Node of int * int * 'a tree * 'a tree



let rec lookup t i = match t with
  | Leaf a -> Some a
  |Node (size,lsize,lt,rt) -> if i <lsize then lookup lt i
  else lookup rt (i-lsize)


let rec update t i x = match t with 
| Leaf a -> if i =0  then Leaf x else raise "TOP G"
| Node(size,lsize,lt,rt) -> if i < lsize then Node (size, lsize, update lt i x, rt)
else Node (size, lsize, lt, update rt (i - lsize) x)


let rec insert t i x = match t with 
| Leaf a ->  Node (2,1, Leaf x,t)
|Node (size,lsize,lr,rt) -> if i < lsize then Node(size+1,lsize+1,insert lr i x,rt) else Node (size+1, lsize, lr,insert rt i x)


let rec list_of_tree t = match t with
  | Leaf x -> [x]
  | Node (_, _, lt, rt) -> list_of_tree lt @ list_of_tree rt