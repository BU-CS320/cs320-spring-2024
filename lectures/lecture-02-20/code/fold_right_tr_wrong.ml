let fold_right_tr op l base =
  let rec go l acc =
    match l with
    | [] -> acc
    | x :: xs -> go xs (op acc x)
  in go l base

let _ = assert (fold_right_tr (+) [1;2;3;4;5] 0 = 15)
let _ = assert (fold_right_tr (@) [[1];[2];[3];[4]] [] = [1;2;3;4])

(*
fold_right (+) [1;2;3] 0            ==>
1 + fold_right (+) [2;3] 0          ==>
1 + (2 + fold_right (+) [3] 0)      ==>
1 + (2 + (3 + fold_right (+) [] 0   ==>
1 + (2 + (3 + 0))                   ==>
1 + (2 + 3)                         ==>
1 + 5                               ==>
6
*)

(*
fold_right_tr (+) [1;2;3] 0   ==>
go [1;2;3] 0                  ==>
go [2;3] (0 + 1)              ==>
go [2;3] 1                    ==>
go [3] (1 + 2)                ==>
go [3] 3                      ==>
go [] (3 + 3)                 ==>
go [] 6                       ==>
6
*)