let filter = assert false (* TODO *)
let append = assert false (* TODO *)





















(* A SOLUTION *)
let filter p = List.fold_right (fun x xs -> (if p x then [x] else []) @ xs) []
let append l r = List.fold_right (fun x xs -> x :: xs) r l