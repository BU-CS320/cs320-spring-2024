
(* BEGINNING OF UTILS *)

let cons x xs = x :: xs
let explode (s : string) : char list = String.fold_right cons s []
let implode (cs : char list) : string = String.init (List.length cs) (List.nth cs)
let is_digit (c : char) : bool = List.mem c (explode "0123456789")
let is_whitespace (c : char) : bool = List.mem c (explode " \r\n\t")

let rec take_while (p : 'a -> bool) (l : 'a list) =
  match l with
  | [] -> []
  | (x :: xs) -> if p x then x :: take_while p xs else []

let rec drop_while (p : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | (x :: xs) -> if p x then drop_while p xs else x :: xs

(* END OF UTILS *)

(* BEGINNING OF TOKENIZING *)

(* Simple Grammar

   <expr>  ::= <expr2> {( + | - ) <expr2>}
   <expr2> ::= <expr3> {( * | / ) <expr3>}
   <expr3> ::= <int> | ( <expr> )
   <int>   ::= <digit> {<digit>}
   <digit> ::= 0|1|2|3|4|5|6|7|8|9

*)

type token = Unit (* TODO *)

let rec next_token (cs : char list) : (token * char list) option = assert false (* TODO *)

let tokenize
    (next : char list -> ('a * char list) option)
    (s : string) : token list option =
  assert false (* TODO *)

(* END OF TOKENIZING *)

(* BEGINNING OF PARSING *)

(* A Simpler Grammar (right-associative addition with parentheses)

   <expr> = <expr2> | <expr2> + <expr2>
   <expr> = <int> | ( <expr> )

*)

type expr
  = Num of int
  | Add of expr * expr

let rec parseExpr (ts : token list) : (expr * token list) option =
  assert false
and parseExpr2 ts =
  assert false

let parse s = assert false

(* END OF PARSING *)
