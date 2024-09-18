type rat = int* int
type var = string
type unary_op = Neg
type binary_op = Add |Sub | Mul | Division
type expr = Const of rat 
            | Unop of unary_op * expr
            | Binop of binary_op * expr *expr
            | Var of var
            | Func of var * expr
            | Bind of var * expr * expr
            | App of expr * expr
            | Ite of expr * expr * expr
type value = Rat of rat | Fun of var * state * expr
and state = var -> value option



let rec eval_expr = function Const f -> f
    | Unop (Neg, e) -> let n,d = eval_expr e in -n, d
    | Binop (op, e1, e2) ->
      let (n1,d1) = eval_expr e1 in
      let (n2,d2) = eval_expr e2 in
      match op with
      | Add -> (n1*d2+n2*d1,d1*d2)
      | Sub -> (n1*d2-n2*d1,d1*d2)
      | Mul -> (n1*n2,d1*d2)
      | Division -> (n1*d2,d1*n2)
