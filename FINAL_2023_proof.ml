  let rec app x y = match x with
  | [] ->  y
  | h::t -> h:: app t  y


  let rec rev a x = match x with 
  | [] -> a
  | h::t-> rev(h::a) t

  let rec apprev x y = match x with 
  | [] -> rev [] y 
  | h::t -> app (apprev t y ) [h]
