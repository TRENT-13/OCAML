type nat = Zero | Succ of nat

let rec int_to_nat i = if i <= 0 then Zero else Succ(int_to_nat (i-1))

 
let rec nat_to_int i = match i with
| Zero -> 0
| Succ x -> 1 + nat_to_int x

 
let rec add x y = match x with Zero -> y
 | Succ x' -> add x' (Succ y)



 let rec mul x y = match x with Succ x' -> add y (mul x' y) | Zero -> Zero


let rec pow x y = match x with Zero -> Zero
| Succ x' -> mul y (pow x' y)

let rec comp x y =  match x, y with  Succ x', Succ y' -> comp x' y'
| _ -> x = Zero

