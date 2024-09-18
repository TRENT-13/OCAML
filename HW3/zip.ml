let unzip lst =
  List.fold_right (fun (x1, x2) (acc1, acc2) -> (x1 :: acc1, x2 :: acc2)) lst ([], [])
 