module Pairs :
sig
  type 'a pair = 'a * 'a
  val pair : 'a * 'a -> 'a * 'a
  val first : 'a * 'a -> 'a
  val second : 'a * 'a -> 'a
end = struct
  type 'a pair = 'a * 'a
  let pair (a,b) = (a,b)
  let first (a,b) = a
  let second (a,b) = b
end


module Triples :
sig
type 'a triple = Triple of 'a * 'a * 'a
val first : 'a triple -> 'a
val second : 'a triple -> 'a
val third : 'a triple -> 'a
end  = struct
  type 'a triple  = Triple of 'a * 'a * 'a 
  let first  (Triple (a,_,_)) = a
  let second (Triple (_,b,_)) = b
  let third (Triple (_,_,c)) = c
end


module A = struct
  include Triples
include Pairs
end


module Quads = struct
  module Pairs = struct
    type 'a pair = 'a * 'a 
    let pair (a,b) = (a,b)
    let first (a,b) = a
    let second (a,b) = b
  end

  type 'a quad = 'a Pairs.pair Pairs.pair

  let quad (a,b,c,d) = Pairs.pair(Pairs.pair(a,b), Pairs.pair(c,d))
  let first q = Pairs.first (Pairs.first q)
  let second q = Pairs.second (Pairs.first q)
  let third q = Pairs.first(Pairs.second q)
  let fourth q = Pairs.second (Pairs.second q)
end

