(*
   Write a tokenizer for regular expressions on the terminal symbols 0
   and 1.

   That is, define a `token` type and write the `next_token` function
   which, given a list of characters, returns the token for the next
   lexeme in the list, as well as the remainder of the list.
*)

let explode (s : string) : char list = String.fold_right List.cons s []
let is_whitespace (c : char) : bool = List.mem c (explode " \r\n\t")

type token = TZero | TOne | TLPar | TRPar | TStar | TBar

let rec next_token (cs : char list) : (token * char list) option =
  match cs with
  | '0' :: rest -> Some (TZero, rest)
  | '1' :: rest -> Some (TOne, rest)
  | '(' :: rest -> Some (TLPar, rest)
  | ')' :: rest -> Some (TRPar, rest)
  | '*' :: rest -> Some (TStar, rest)
  | '|' :: rest -> Some (TBar, rest)
  | c :: rest when is_whitespace c -> next_token rest
  | _ -> None

let rec tokenize (cs : char list) : token list option =
  if cs = [] then Some []
  else
    match next_token cs with
    | None -> None
    | Some (t, rest) -> (
        match tokenize rest with None -> None | Some ts -> Some (t :: ts))

let tokenize_s (s : string) : token list option = tokenize (explode s)

let () =
  assert (
    tokenize_s "(01)*01*"
    = Some [ TLPar; TZero; TOne; TRPar; TStar; TZero; TOne; TStar ])

let () = assert (tokenize_s "(01)*01*q" = None)
