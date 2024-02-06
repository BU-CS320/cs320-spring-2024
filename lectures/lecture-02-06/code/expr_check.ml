(* Variant for arithemtic expressions *)

type expr
  = Val of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr

let x = Add (Val 3, Sub (Mul (Val 2, Val 4), Val 14))

let rec eval (e : expr) : int = assert false

let _ = assert (eval x = -3)

