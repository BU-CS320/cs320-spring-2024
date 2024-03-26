(* UTILITY FUNCTIONS *)

let explode s = List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)
let is_blank c = String.contains " \012\n\r\t" c
let is_digit c = '0' <= c && c <= '9'

(* DEFINITION OF A PARSER *)

type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : 'a option = Option.map fst (p (explode s))

(* BASIC INPUT READING *)

let char (c : char) : unit parser = function
  | d :: cs -> if c = d then Some ((), cs) else None
  | _ -> None

let satisfy (p : char -> bool) : char parser = function
  | c :: cs when p c -> Some (c, cs)
  | _ -> None

let str (s : string) : unit parser =
  let rec go ds cs =
    match ds, cs with
    | d :: rest1, c :: rest2 when d = c -> go rest1 rest2
    | [], rest -> Some ((), rest)
    | _ -> None
  in go (explode s)

(* MAPPING *)

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  fun cs ->
  Option.map (fun (s, rest) -> (f s, rest)) (p cs)

(* infix map operation with arguments flipped *)
let (>|=) p f = map f p

(* EXAMPLE *)

let token (s : string) (tok : 'a) : 'a parser = map (fun _ -> tok) (str s)

(* infix map operation with arguments flipped whose
   function is always the constant function
*)
let (>|) = fun p c -> map (fun _ -> c) p

let token s tok = (str s) >| tok

(* SEQUENCING *)

let seq (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser = fun cs ->
  match p1 cs with
  | None -> None
  | Some (a, rest) ->
    match p2 rest with
    | None -> None
    | Some (b, rest) -> Some ((a, b), rest)

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

(* ALTERNATIVES *)

let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser = fun cs ->
  match p1 cs with
  | Some (x, rest) -> Some (x, rest)
  | None ->
    match p2 cs with
    | Some (x, rest) -> Some (x, rest)
    | None -> None

(* infix alternative operator *)
let (<|>) = disj

(* EXAMPLE *)

type token
  = AddT
  | SubT
  | MulT
  | DivT

let arith_op =
  (char '+' >| AddT) <|>
  (char '-' >| SubT) <|>
  (char '*' >| MulT) <|>
  (char '/' >| DivT)

(* OPTIONALS *)

let optional (p : 'a parser) : 'a option parser =
  fun cs ->
  match p cs with
  | Some (x, rest) -> Some (Some x, rest)
  | None -> Some (None, cs)

(* EXAMPLE *)

(* <tm> ::= if <tm> then <tm> [else <tm>] *)
type tm
  = Var
  | IFTHENELSE of tm * tm * (tm option)

let if_then_opt_else p1 p2 p3 =
  seq3
    (str "if "    >> p1)
    (str " then " >> p2)
    (optional (str " else " >> p3))
  >|= (fun (b, x, opy) -> IFTHENELSE (b, x, opy))

let var = char 'x' >| Var

let test = parse (if_then_opt_else var var var) "if x then x"
let _ = assert (test = Some (IFTHENELSE (Var,Var,None)))

(* note: doesn't handle whitespace well *)

(* PURE AND FAILURE *)

let pure (x : 'a) : 'a parser = fun cs -> Some (x, cs)
let fail : unit parser = fun _ -> None
let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  fun cs ->
  match p cs with
  | Some (x, rest) -> f x rest
  | None -> None

let (>>=) = bind
let ( let* ) = bind

(* EXAMPLE *)

let guard (b : bool) : unit parser = if b then pure () else fail

let pos_digit =
  satisfy is_digit >>= (fun c -> guard (c <> '0'))

let pos_digit =
  let* c = satisfy is_digit in
  guard (c <> '0')

(* REPETITION *)

let rec many (p : 'a parser) : 'a list parser = fun cs ->
  match p cs with
  | None -> Some ([], cs)
  | Some (x, rest) ->
    match many p rest with
    | None -> Some ([x], rest)
    | Some (xs, rest) -> Some (x :: xs, rest)

let rec many1 (p : 'a parser) : 'a list parser =
  map2 (fun x xs -> x :: xs) p (many p)

(* EXAMPLES *)

let ws = many (satisfy is_blank) >> pure ()
let keyword wd = str wd >> ws

let nat =
  many1 (satisfy is_digit) >|=
  fun cs -> int_of_string (implode cs)

let ocaml_zeros_list =
  let body = many (char '0' >> ws >> keyword ";") in
  let last = optional (char '0' >> ws) in
  let all = keyword "[" >> (seq body last) << keyword "]" in
  all >|= fun (xs, ol) ->
    (List.map (fun _ -> 0) xs) @
    (if Option.is_none ol then [] else [0])

let test = parse ocaml_zeros_list "[0;0;0]"
let _ = assert(test = Some [0;0;0])
let test = parse ocaml_zeros_list "[0;0;0;]"
let _ = assert(test = Some [0;0;0])
let test = parse ocaml_zeros_list "[0;0;0;;]"
let _ = assert(test = None)

type tm
  = Var
  | IFTHENELSE of tm * tm * tm

let if_then_else_ p1 p2 p3 =
  map3
    (fun b x y -> IFTHENELSE (b, x, y))
    (keyword "if"   >> p1)
    (keyword "then" >> p2)
    (keyword "else" >> p3)

(* EXTENDED EXAMPLE: TOKENIZING *)

type tok =
  | LPar
  | RPar
  | AddT
  | MulT
  | NumT of int

let tokenize =
  let next_token =
    (keyword "(" >| LPar) <|>
    (keyword ")" >| RPar) <|>
    (keyword "+" >| AddT) <|>
    (keyword "*" >| MulT) <|>
    (nat << ws >|= (fun n -> NumT n))
  in many next_token

let test = parse (ws >> tokenize) "  01 ))+2* 234 "
let _ = assert (test = Some [NumT 1; RPar; RPar; AddT; NumT 2; MulT; NumT 234])

(* EXTENDED EXAMPLE: EXPRESSIONS *)

(* <expr> ::= <term> { + <term> } *)
let chain_left_one_op op parse_op parse_arg =
  let first_arg = parse_arg in (* <term> *)
  let rest = many (parse_op >> parse_arg) in (* { + <term> } *)
  map2 (List.fold_left op) first_arg rest (* apply op between each term *)

type expr =
  Num of int
  | Mul of expr * expr
  | Add of expr * expr

let rec parse_expr : expr parser =
  let base = (nat << ws >|= (fun x -> Num x))in
  let muls = chain_left_one_op (fun x y -> Mul (x, y)) (keyword "*") base in
  let adds = chain_left_one_op (fun x y -> Add (x, y)) (keyword "+") muls in
  adds

let test = parse parse_expr "1 * 3 + 4 + 2"
let out = Add (Add (Mul (Num 1, Num 3), Num 4), Num 2)
let _ = assert (test = Some out)

let rec parse_expr () : expr parser =
  (* let pars =
    let* _ = keyword "(" in
    let* e = parse_expr () in
    let* _ = keyword ")" in
    pure e
  in *)
  let pars =
    let* _ = pure () in
    keyword "(" >> parse_expr () << keyword ")"
  in
  let base = (nat << ws >|= (fun x -> Num x)) <|> pars in
  let muls = chain_left_one_op (fun x y -> Mul (x, y)) (keyword "*") base in
  let adds = chain_left_one_op (fun x y -> Add (x, y)) (keyword "+") muls in
  adds

let test = parse (parse_expr ()) "1 * (3 + 4) + 2"
let out = Add (Mul (Num 1, (Add (Num 3, Num 4))), Num 2)
let _ = assert (test = Some out)

