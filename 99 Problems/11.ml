type 'a rle =
    | One of 'a
    | Many of int * 'a;;


let enncode l = 
  let create_a_tuple cnt el = 
    if cnt =1 then One el else Many (cnt, el)
  in 
  let rec aux count acc = function
  | [] -> []
  | [x] -> (create_a_tuple (count+1) x) ::acc
  | hd :: (snd :: _ as t) -> if hd = snd then aux (count+1) acc t 
  else aux 0 ((create_a_tuple(count+1)hd) ::acc) t
in
List.rev (aux 0 [] l)
