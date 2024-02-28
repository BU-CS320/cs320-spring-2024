(* Every n elements

   Implement the function `every_n` which, given

     n : (positive) int
     l : 'a list

   returns every `n` elements of `l`, starting with the first element.
   You should implement this using List.fold_left.


   Examples:
   let _ = assert (every_n 1 [1;2;3;4;5] = [1;2;3;4;5])
   let _ = assert (every_n 2 [1;2;3;4;5] = [1;3;5])
   let _ = assert (every_n 3 [1;2;3;4;5] = [1;4])
   let _ = assert (every_n 4 [1;2;3;4;5] = [1;5])
   let _ = assert (every_n 5 [1;2;3;4;5] = [1])

*)

let every_n (n : int) (l : 'a list) : 'a list =
  assert false (* TODO *)
