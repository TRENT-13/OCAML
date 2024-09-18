type 'a llist = Cons of 'a * (unit -> 'a llist)

let rec fib_gen x =
  if x <= 2 then 1 else fib_gen (x - 1) + fib_gen (x - 2)

let lazy_impl =
  let rec gen_seq from = Cons (from, fun () -> gen_seq (from - 1)) in
  gen_seq (fib_gen 9) 
 


(*            solutions          *)

let rec ltake n (Cons (h, t)) =
  if n <= 0 then [] else h::ltake (n-1) (t ())

  
type 'a llist = Cons of 'a * (unit -> 'a llist)

let rec lnat i = Cons (i, (fun () ->  lnat (i + 1))) 


let lfib () =
let rec impl a b = Cons (a, fun () -> impl b (a+b))
in
impl 0 1


let rec ltake n (Cons (h, t)) =
if n <= 0 then [] else h::ltake (n-1) (t ()) 


let rec lfilter f (Cons (h, t)) =
if f h then Cons (h, fun () -> lfilter f (t ()))
else lfilter f (t ()) 

