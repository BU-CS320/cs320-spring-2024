(* UTILITY FUNCTIONS *)

let explode s = List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)
let is_blank c = String.contains " \012\n\r\t" c
let is_digit c = '0' <= c && c <= '9'

(* DEFINITION OF A PARSER *)

type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : 'a option = Option.map fst (p (explode s))

(* BASIC INPUT READING *)

let char (c : char) : char list -> (unit * char list) option =
  fun cs ->
  match cs with
  | d :: ds when c = d -> Some ((), ds)
  | _ -> None

let test = parse (char 'c') "c"
let _ = assert (test = Some ())
let test = parse (char 'c') "d"
let _ = assert (test = None)

let satisfy (f : char -> bool) : char parser =
  fun cs ->
  match cs with
  | d :: ds when f d -> Some (d, ds)
  | _ -> None

let char c = satisfy ((=) c)

let str (s : string) : unit parser =
  let rec go ds cs =
    match ds, cs with
    | d :: rest1, c :: rest2 when d = c -> go rest1 rest2
    | [], rest -> Some ((), rest)
    | _ -> None
  in go (explode s)

let test = parse (str "let") "let"
let _ = assert (test = Some ())
let test = parse (str "let") "  let"
let _ = assert (test = None)

(* MAPPING *)

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  fun cs ->
  match p cs with
  | Some (x, rest) -> Some (f x, rest)
  | _ -> None

(* infix map operation with arguments flipped *)
let (>|=) p f = map f p

(* EXAMPLE *)

let token (tok : 'a) (name : string) : 'a parser =
  str name >|= (fun _ -> tok)

let test = parse (token 0 "zero") "zero"
let _ = assert (test = Some 0)

(* infix map operation with arguments flipped whose
   function is always the constant function
*)
let (>|) = fun p c -> map (fun _ -> c) p

let token tok name = str name >| tok

(* SEQUENCING *)

let seq (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
  fun cs ->
  match p1 cs with
  | Some (x, rest) ->
    (match p2 rest with
     | Some (y, rest) -> Some ((x, y), rest)
     | None -> None)
  | None -> None

(* Infix sequencing operators with chosen output *)
let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

(* BOOSTRAPPING SEQUENCING AND MAPPING *)

let seq2   p1 p2 = seq p1 p2
let map2 f p1 p2 = map (fun (x, y) -> f x y) (seq p1 p2)

let seq3   p1 p2 p3 = map2 (fun a (b, c) -> (a, b, c)) p1 (seq p2 p3)
let map3 f p1 p2 p3 = map  (fun (a, b, c) -> f a b c) (seq3 p1 p2 p3)

let seq4   p1 p2 p3 p4 = map3 (fun a b (c, d) -> (a, b, c, d)) p1 p2 (seq p3 p4)
let map4 f p1 p2 p3 p4 = map  (fun (a, b, c, d) -> f a b c d) (seq4 p1 p2 p3 p4)

let between (opn : 'a parser) (cls : 'a parser) (p : 'b parser) =
  opn >> p << cls

let nonterm = char '<' >> str "test" << char '>'
let test = parse nonterm "<test>"
let _ = assert (test = Some ())

(* ALTERNATIVES *)

let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun cs ->
  match p1 cs with
  | Some (x, rest) -> Some (x, rest)
  | None ->
    (match p2 cs with
     | Some (y, rest) -> Some(y, rest)
     | None -> None)

(* infix alternative operator *)
let (<|>) = disj

(* EXAMPLE *)

type token
  = AddT
  | SubT
  | MulT
  | DivT

let arith_op =
  (token AddT "+") <|>
  (token SubT "-") <|>
  (token MulT "*") <|>
  (token DivT "/")


(* OPTIONALS *)

let optional p =
  fun cs ->
  match p cs with
  | Some (x, rest) -> Some (Some x, rest)
  | None -> Some (None, cs)

(* EXAMPLE *)

(* <tm> ::= if <var> then <var> [else <var>] *)

type tm
  = Var
  | IFTHENELSE of tm * tm * (tm option)

let var = char 'x' >| Var

let if_then_opt_else : tm parser =
  map3
    (fun b x oy -> IFTHENELSE (b, x, oy))
    (str "if " >> var)
    (str " then " >> var)
    (optional (str " else " >> var))

let test = parse if_then_opt_else "if x then x"
let _ = assert (test = Some (IFTHENELSE (Var,Var,None)))
let test = parse if_then_opt_else "if x then x else x"
let _ = assert (test = Some (IFTHENELSE (Var,Var,Some Var)))

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

let rec many (p : 'a parser) : 'a list parser =
  fun cs ->
  match p cs with
  | Some (x, rest) ->
    (match many p rest with
     | Some (xs, rest) -> Some (x :: xs, rest)
     | None -> Some ([x], rest))
  | None -> Some ([], cs)
let many1 = 0

(* EXAMPLES *)

let ws = many (satisfy is_blank)
let keyword wd = str wd << ws

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

let tokenize =
  let next_token =
    token LPar "(" <|>
    token RPar ")" <|>
    token AddT "+" <|>
    token MulT "*" <|>
    (many (satisfy is_digit) >|=
     (fun cs -> NumT (int_of_string (implode cs))))
   many next_token

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
