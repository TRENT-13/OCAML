let sum lst = 
  List.fold_left (fun acc x -> acc * x)  1 lst;;


  let max_number_list lst = 
    List.fold_left max (List.hd lst) lst;;



let rev_list lst = 
  List.fold_right (fun x acc -> x @ acc) [] lst;; 


let in_lst lst = 
  List.fold_left (fun x-> List.mem x) lst ;;


let concationaaa lst = 
  List.fold_left (fun x acc -> x @ acc) [] lst;;


let convert_nigger lst=
  List.fold_left (fun x acc -> x ^ acc) "" lst;; 

let filter_nigger lst = 
  List.fold_left (fun acc x -> if p x then acc @ [x] else acc) [] ;;