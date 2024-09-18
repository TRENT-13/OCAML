module type LIST_STACK = sig
  exception Empty
  val empty : 'a list
  val is_empty : 'a list -> bool
  val push : 'a -> 'a list -> 'a list
  val peek : 'a list -> 'a
  val pop : 'a list -> 'a list
end


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

module ListStack : LIST_STACK = struct
  let empty = []

  let is_empty = function [] -> true | _ -> false

  let push x s = x :: s

  exception Empty

  let peek = function
    | [] -> raise Empty
    | x :: _ -> x

  let pop = function
    | [] -> raise Empty
    | _ :: s -> s
end

let s = ListStack.empty
let s' = ListStack.push 1 s                                              


let s1 : int list = ListStack.(empty |> push 42)
let s2 : int list = [42]