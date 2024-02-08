(* Split (Warm Up)

   Implement a function `split` which given

     l : an 'a list
     index : an integer

   returns a pair of lists `(a, b)` such that

     * `a @ b` is the same as `l`
     * if `l` is length n, then length of `a` is min(max(0, index), n)

   Note to TA/TFs and myself: Emphasize that this can be done without
   List.rev.
*)

let rec split_at (l : 'a list) (index : int) : 'a list * 'a list =
  match l with
  | [] -> [], []
  | x :: xs ->
    if index <= 0 then
      [], x :: xs
    else
      let (ls, rs) = split_at xs (index - 1) in
      x :: ls, rs

let _ = assert (split_at [1;2;3] (-10) = ([], [1;2;3]))
let _ = assert (split_at [1;2;3;4;5] 2 = ([1;2], [3;4;5]))
let _ = assert (split_at [1;2;3;4;5] 10 = ([1;2;3;4;5], []))
