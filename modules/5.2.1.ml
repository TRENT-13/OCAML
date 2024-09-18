module M = struct
  let x = 0;;
  type t = int;;
end


module N = struct let x = 0 let y = 1 end
module O = struct let x = 0;; let y = 1 end


module M = struct
  (* Requires: input is non-negative. *)
  let rec even = function 
    | 0 -> true 
    | n -> odd (n - 1)
  and odd = function 
    | 0 -> false 
    | n -> even (n - 1)
end