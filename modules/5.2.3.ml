module MyStack = struct
  type 'a stack = 
    | Empty
    |Entry of 'a * 'a stack

  let empty  = Empty

  let push x s = Entry (x,s)

  let peek = function
  | Empty -> Empty
  | Entry (x, _) -> x

  let pop = function
  | Empty -> Empty
  | Entry (_, s) -> s
end

module ListStack = struct
  type 'a stack = 'a list

  let empty = []
  let push x s= x::s  
  
  let peek = function
  | [] -> failwith "Empty nigger"
  | h :: _ -> h


  let pop = function
  | [] -> failwith "empty"
  | _:: t  ->  t
  
end

let x = ListStack.(peek (push 42 []))     

let y = ListStack.(empty |> push 32 |> peek)

let w = 
let open ListStack in empty |> push 32 |> peek


open ListStack
let v  = empty |> push 32 |> peek


open MyStack 
let  u = empty