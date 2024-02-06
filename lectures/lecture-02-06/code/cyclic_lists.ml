(* Examples of cyclic lists *)

(*
let not_good =
  let rec go i = i :: go (i + 1) in go 0
*)
let rec what_about_this = 1 :: 2 :: what_about_this