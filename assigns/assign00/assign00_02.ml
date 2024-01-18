(* Primality check

   Please implement a function `is_prime` of type `int -> bool` such that
   when given an integer `n` greater than or equal to 0, it returns a boolean value.
   If the input is prime return true, otherwise return false.

   Note 1: 0 and 1 are not considered prime numbers.
   Note 2: In OCaml, the modulo operation is written a `4 mod 3`. This specific instance will result in 1. 

   Examples:
   is_prime 0 = false
   is_prime 1 = false
   is_prime 2 = true
   is_prime 37 = true
   is_prime 57 = false
   is_prime 97 = true

*)

let is_prime (n : int) : bool =
  (* YOUR CODE GOES HERE *)