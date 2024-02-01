(* Modular Arithmetic (Written Problem)

   Implement the function `modulo` of type `int -> int -> int` which,
   given

   n : a nonnegative integer
   k : a positive integer

   returns the value `n mod k` WITHOUT using the operator `(mod)`.

   Bonus Question: Is your solution tail recursive? Explain why or why not.

   Note to TF/TAs: Breifly talk about what makes a function
   tail-recursive when going over the solution.
*)

let rec modulo (n : int) (k : int) : int =
  if n >= k then
    modulo (n - k) k
  else
    n

let _ = assert (modulo 1048 234 = 1048 mod 234)
let _ = assert (modulo 1234 113 = 1234 mod 113)
