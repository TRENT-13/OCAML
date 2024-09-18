let rec map f = function
 | [] -> []
 | h::t ->  let h1 = f h in h1 :: map f t    

 let p x = print_int x; print_newline(); x + 1

 let lst2 = map p [1; 2]




