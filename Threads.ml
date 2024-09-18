let thread_function id =
  for i = 1 to 5 do
    Printf.printf "Thread %d - iteration %d\n%!" id i;
    Thread.delay 0.5
  done

let () =
  let threads = List.init 5 (fun i -> Thread.create thread_function (i + 1)) in
  List.iter Thread.join threads;
  Printf.printf "All threads finished\n%!"
