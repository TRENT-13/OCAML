module ListMOdule =  struct
  type t

  let hd lst = List.map(fun x -> List.hd) lst 

  let tl lst = List.map (fun x -> List.rev x ) 
   let tl_1 tl = List.map (fun x -> List.hd x)

   let rec length lst  = function
   | [] -> 0
   | h::t -> 1+ length t

   let rec app l1 l2 = function
   | [] -> l2
   | h::t -> h:: app t l2

   let rec rev lst a = function
   | [] -> a
   | h::t -> rev t (h::a)

   let rec nth lst i = function
   | [] -> failwith "sii"
   | h::t -> if i=0 then Some h else nth t (i-1) 
   

end