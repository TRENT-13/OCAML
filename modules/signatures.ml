module type Fact  = sig
  val fact : int -> int

end

module RecursiveFact : Fact = struct
  let rec fact  n = 
    if n=0 then 1 else n* fact (n-1)
end

(* module Nofact : Fact = struct
  let rec inc n =  inc n/2
end *)


