type door = OPEN of int | CLOSE of int


let toggle door = match door with 
| OPEN a -> CLOSE a
|CLOSE a-> OPEN a

let  gen_doors n  = 
  let rec aux a n = if n <= 0 then a else aux (CLOSE(n) ::a) (n-1)
in aux [] n




let pass n lst =
  let rec aux i lst acc = match lst with
    | [] -> List.rev acc
    | h :: t -> 
        if (i mod n) = 0 then 
          aux (i + 1) t (toggle h :: acc) 
        else 
          aux (i + 1) t (h :: acc)
  in
  aux 1 lst []

let passes n lst =
  let rec aux k lst =
    if k > n then lst
    else aux (k + 1) (pass k lst)
  in
  aux 1 lst