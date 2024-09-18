let rec app = fun x-> fun y-> match x
 with []-> y
 | h::t-> h :: app t y



 let rec rev = fun x-> match x
with []-> []
| h::t-> app (rev t) [h]
