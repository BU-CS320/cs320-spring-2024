(* Implementation of Options *)

type 'a myoption = None | Some of 'a

let head (l : 'a list) : 'a myoption =
  match l with
  | [] -> None
  | x :: xs -> Some x
