module type geometry = sig
  type point
  type shape 

  val rectangle_area = point -> point -> float
  val square_area = point -> point ->  float
  val triangle_area = point -> point -> point -> float
end

module geometrys : geometry  = struct
  type point = float *float
  type shape = rectangle | triangle |square
end
  
(* d=√((x2 – x1)² + (y2 – y1)²) *) 
let distance_calc (x1,y1) (x2,y2) =
  let dx = x2 -. x1 and
  let dy = y2 -. y2  and
  sqrt (dx *. dx +. dy *. dy)

