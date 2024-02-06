(* Simple recursive variant  *)

type t
  = A
  | B of t
  | C of t * t

let example : t = B (C (A, B A))