let rec app l y = match l with
| [] -> y
| x::xs -> x :: app xs y;;