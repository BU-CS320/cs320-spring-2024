(*
   Write a tokenizer for regular expressions on the terminal symbols 0
   and 1.

   That is, define a `token` type and write the `next_token` function
   which, given a list of characters, returns the token for the next
   lexeme in the list, as well as the remainder of the list.
*)

type token = Unit (* TODO: Fix this *)

let next_token (cs : char list) : (token * char list) option =
  assert false (* TODO *)

let tokenize (cs : char list) : (token list) option =
  assert false (* TODO *)
