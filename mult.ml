let double x = 2 * x;;

let result = (double 3, double (double 1));;
print_endline ("Result: " ^ string_of_int (fst result) ^ ", " ^ string_of_int (snd result));;
