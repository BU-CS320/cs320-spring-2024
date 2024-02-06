(* There is no function overloading in OCaml *)

let add (a : int) (b : int) : int = a + b
let add (a : string) (b : string) : string = a ^ b (* This overwrite above *)
let add (a : 'a list) (b : 'a list) : 'a list = a @ b (* This overwrites above *)