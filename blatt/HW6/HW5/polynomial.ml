let rec eval_poly x coeffs =
  match coeffs with
  | [] -> 0.0 
  | h :: t -> h +. x *. eval_poly x t


  let rec derive_poly lst =
    let rec length lst acc = match lst with
      | [] -> acc
      | h::t -> length t (acc + 1) in
    let rec res length lst = match lst with
      | [] -> []
      | h::t -> h *  length :: res  (length - 1) t in
    res (length lst 0) lst ;;
  
    
    let eval_poly x coeffs =
      let rec impl value coeffs =
        match coeffs with [] -> value
        | c::cs -> impl ((value *. x) +. c) cs
      in
      impl 0.0 coeffs
    
    let derive_poly coeffs =
      let rec impl = function [] | [_] -> ([], 0.)
      | c::cs -> let (new_coeffs, deg) = impl cs in
        (c *. (deg +. 1.))::new_coeffs, deg +. 1.
      in fst (impl coeffs)