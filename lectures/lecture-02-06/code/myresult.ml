(* Implementation of Results *)

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

let head (l : 'a list) : ('a, string) result =
  match l with
  | [] -> Error "[] has no first element"
  | x :: xs -> Ok x