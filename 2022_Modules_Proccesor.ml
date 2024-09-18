module type P = sig
  type prog
  type c
  val init_conf : unit -> c
  val run : c -> prog -> c
  val return : c -> string
  end


type arith_instr = Set of int | Swap | Neg | Add
type conf = {x:int; y:int}


module AP = struct
  type prog = arith_instr
  type c = conf

  let init_conf () = {x=0; y =0}

  let return c = string_of_int c.x

  let run prog c = match prog with 
  | Set v -> {x = v; y=c.y}
  | Swap -> {x=c.y; y=c.x} 
  | Neg -> {x= -c.x; y= -c.y}
  | Add -> {x=c.x+ c.y; y=c.y}
end 


type ('c,'p) control_flow = Base of 'p
| Sequence of ('c,'p) control_flow list
| Condition of ('c -> bool) *
('c,'p) control_flow *
('c,'p) control_flow
| Loop of ('c -> bool) * ('c,'p) control_flow

module CP(Base:P) = struct
type prog = (Base.c,Base.prog) control_flow
type c = Base.c
let init_conf () = Base.init_conf ()
let return c = Base.return c
let rec run c = function
| Base prog -> Base.run c prog
| Sequence list -> List.fold_left run c list
| Condition (p,prog1,prog2) -> if p c then run c prog1
else run c prog2
| Loop (p,prog) -> if p c then run (run c prog) (Loop (p,prog))
else c
end

