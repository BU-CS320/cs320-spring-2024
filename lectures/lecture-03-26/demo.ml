(* UTILITY FUNCTIONS *)

let explode s = List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)
let is_blank c = String.contains " \012\n\r\t" c
let is_digit c = '0' <= c && c <= '9'

(* DEFINITION OF A PARSER *)

type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : 'a option = Option.map fst (p (explode s))

(* BASIC INPUT READING *)

let char = 0

let satisfy = 0

let str (s : string) : unit parser =
  let rec go ds cs =
    match ds, cs with
    | d :: rest1, c :: rest2 when d = c -> go rest1 rest2
    | [], rest -> Some ((), rest)
    | _ -> None
  in go (explode s)

(* MAPPING *)

let map = 0

(* infix map operation with arguments flipped *)
(* let (>|=) p f = map f p *)

(* EXAMPLE *)

let token = 0

(* infix map operation with arguments flipped whose
   function is always the constant function
*)
(* let (>|) = fun p c -> map (fun _ -> c) p *)

let token = 0

(* SEQUENCING *)

let seq = 0

(* Infix sequencing operators with chosen output *)
(* let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2) *)

(* BOOSTRAPPING SEQUENCING AND MAPPING *)

(* let seq2   p1 p2 = seq p1 p2
let map2 f p1 p2 = map (fun (x, y) -> f x y) (seq p1 p2)

let seq3   p1 p2 p3 = map2 (fun a (b, c) -> (a, b, c)) p1 (seq p2 p3)
let map3 f p1 p2 p3 = map  (fun (a, b, c) -> f a b c) (seq3 p1 p2 p3)

let seq4   p1 p2 p3 p4 = map3 (fun a b (c, d) -> (a, b, c, d)) p1 p2 (seq p3 p4)
let map4 f p1 p2 p3 p4 = map  (fun (a, b, c, d) -> f a b c d) (seq4 p1 p2 p3 p4) *)

let between = 0

(* ALTERNATIVES *)

let disj = 0

(* infix alternative operator *)
(* let (<|>) = disj *)

(* EXAMPLE *)

type token
  = AddT
  | SubT
  | MulT
  | DivT

let arith_op = 0

(* OPTIONALS *)

let optional = 0

(* EXAMPLE *)

(* <tm> ::= if <tm> then <tm> [else <tm>] *)
type tm
  = Var
  | IFTHENELSE of tm * tm * (tm option)

let if_then_opt_else = 0

(* let var = char 'x' >| Var
let test = parse (if_then_opt_else var var var) "if x then x"
let _ = assert (test = Some (IFTHENELSE (Var,Var,None))) *)

(* note: doesn't handle whitespace well *)

(* PURE AND FAILURE *)

let pure (x : 'a) : 'a parser = fun cs -> Some (x, cs)
let fail : unit parser = fun _ -> None
let bind = 0

let (>>=) = bind
let ( let* ) = bind

(* EXAMPLE *)

let guard = 0

let pos_digit = 0

(* REPETITION *)

let many = 0
let many1 = 0

(* EXAMPLES *)


let ws = 0
let keyword wd = 0

let ocaml_zeros_list = 0

(* let test = parse ocaml_zeros_list "[0;0;0]"
let _ = assert(test = Some [0;0;0])
let test = parse ocaml_zeros_list "[0;0;0;]"
let _ = assert(test = Some [0;0;0])
let test = parse ocaml_zeros_list "[0;0;0;;]"
let _ = assert(test = None) *)

type tm
  = Var
  | IFTHENELSE of tm * tm * tm

let if_then_else_ = 0

(* EXTENDED EXAMPLE: TOKENIZING *)

type tok =
  | LPar
  | RPar
  | AddT
  | MulT
  | NumT of int

let tokenize = 0

(*
let test = parse (ws >> tokenize) "  01 ))+2* 234 "
let _ = assert (test = Some [NumT 1; RPar; RPar; AddT; NumT 2; MulT; NumT 234]) *)

(* EXTENDED EXAMPLE: EXPRESSIONS *)

(* <expr> ::= <term> { + <term> } *)
let chain_left_one_op = 0

type expr =
  Num of int
  | Mul of expr * expr
  | Add of expr * expr

let rec parse_expr = 0

(* let test = parse parse_expr "1 * 3 + 4 + 2"
let out = Add (Add (Mul (Num 1, Num 3), Num 4), Num 2)
let _ = assert (test = Some out) *)

let rec parse_expr = 0

(* let test = parse (parse_expr ()) "1 * (3 + 4) + 2"
let out = Add (Mul (Num 1, (Add (Num 3, Num 4))), Num 2)
let _ = assert (test = Some out) *)