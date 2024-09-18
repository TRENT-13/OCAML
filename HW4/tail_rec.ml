let rec map_tail f acc = function
| [] -> acc
| h :: t -> map_tail f (f h :: acc) t