let rec member c t = function
  | [] -> false
  | x :: xs -> c t x = 0 || member c t xs
