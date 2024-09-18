let rec len lst = function
| [] -> 0
| h::t -> 1+ len t