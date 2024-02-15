(* Matrix-vector multiplication

   Note to TF/TAs and myself: if you have time, go over how to deal
   with errors using options or results.

   Implement the function `mv_mul` which, given

     a : a list of list of floats representing a matrix (as a list of rows)
     v : a list of floats representing a vector

   returns the produce of `a` and `v`.  You may assume that `a` is
   well-formed, and that the multiplication is well-define (i.e., `v`
   has as many entries as `a` does columns.

*)

let rec dot u v =
  match u, v with
  | [], [] -> []
  | x :: xs, y :: ys -> x *. y :: dot xs ys
  | _,_ -> assert false

let rec mv_mul a v =
  match a with
  | [] -> []
  | row :: rows -> dot row v :: mv_mul rows v
