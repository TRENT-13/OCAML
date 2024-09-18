type 'a custom_llist = (unit -> 'a custom_cell)
and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)


let is_Hamming x =
  let rec iterative_div n x =
    if (x mod n == 0) then iterative_div n (x/n) else x in
  iterative_div 5 (iterative_div 3 (iterative_div 2 x)) = 1

  
let hamming_custom =
  let rec from_custom from = fun() -> ConsC (from, from_custom (from + 1)) in
  let rec filter_custom p l =
    fun () ->
    match l () with
    | NilC -> NilC
    | ConsC (h,t) ->
       if p h
       then ConsC (h, filter_custom p t)
       else (filter_custom p t) () in 
  filter_custom is_Hamming (from_custom 1)


  
type 'a ocaml_llist = 'a ocaml_cell Lazy.t
and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)
  


let hamming_ocaml =
  let rec from_ocaml from = lazy (ConsO (from, from_ocaml (from+1))) in
  let rec filter_ocaml p l =
  lazy (
      match Lazy.force l with
      | NilO -> NilO
      | ConsO (h,t) ->
         if p h
         then ConsO (h, filter_ocaml p t)
         else Lazy.force (filter_ocaml p t)
    ) in
  filter_ocaml is_Hamming (from_ocaml 1)