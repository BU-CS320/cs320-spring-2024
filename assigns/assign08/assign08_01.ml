(* UTILITIES *)

let explode s =List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)

let is_lower_case c = 'a' <= c && c <= 'z'
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_alpha c = is_lower_case c || is_upper_case c
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

(* PARSER *)

type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : 'a option =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* BASIC COMBINATORS *)

let satisfy (p : char -> bool) : char parser = function
  | c :: cs when p c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str (s : string) : string parser =
  let rec go ds cs =
    match ds, cs with
    | d :: rest1, c :: rest2 when d = c -> go rest1 rest2
    | [], rest -> Some (s, rest)
    | _ -> None
  in go (explode s)

(* MAPPING *)

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  fun cs -> Option.map (fun (s, rest) -> (f s, rest)) (p cs)

let (>|=) p f = map f p
let (>|) = fun p c -> map (fun _ -> c) p

(* SEQUENCING *)

let seq (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser = fun cs ->
  match p1 cs with
  | None -> None
  | Some (a, rest) ->
    match p2 rest with
    | None -> None
    | Some (b, rest) -> Some ((a, b), rest)

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

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

let (<|>) = disj

(* OPTIONALS *)

let optional (p : 'a parser) : 'a option parser =
  fun cs ->
  match p cs with
  | Some (x, rest) -> Some (Some x, rest)
  | None -> Some (None, cs)

(* PURE, FAILURE, BIND *)

let pure (x : 'a) : 'a parser = fun cs -> Some (x, cs)
let fail : 'a parser = fun _ -> None
let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  fun cs ->
  match p cs with
  | Some (x, rest) -> f x rest
  | None -> None

let (>>=) = bind
let ( let* ) = bind

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

(* USEFUL COMBINATORS *)

let ws = many (satisfy is_blank) >> pure ()
let ws1 = many1 (satisfy is_blank) >> pure ()

let keyword wd = str wd << ws

(* END OF UTILITIES *)

(* ============================================================ *)

(* BEGINNING OF ASSIGNMENT *)

(* Parsing BNF Specifications (Again)

   In this problem, you will be using parser combinators to build a
   better parser for BNF specifications.  In particular, it should
   handle:

   + any sequence of symbols between single quotes as terminal symbols
   + names with dashes in nonterminal symbols
   + alternatives in rules
   + nonterminals, terminals, and symbols '::=' and 'EMPTY' should not
   contain whitespace, but the parser should be whitespace agnostic
   otherwise

   An example of something that should be successfully parsed:

   <expr>       ::= <term> | <expr> '+' <term> | <expr> '-' <term> .
   <term>       ::= <factor> | <term> '*' <factor> | <term> '/' <factor> .
   <factor>     ::= <var-or-num> | '(' <expr> ')' .
   <var-or-num> ::= <var> | <digits> .
   <var>        ::= 'x' | 'y' | 'z' .
   <digits>     ::= <digit> | <digit> <digits> .
   <digit>      ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' .

   The abstract representation of a BNF specification is the same as
   the one from the previous assignment.

   Each of the parser combinators you need to write has a description
   below.

*)

type ident = string
type symbol = T of ident | NT of ident
type sentform = symbol list
type rule = ident * sentform
type grammar = rule list

let ident_of (s : symbol) : ident =
  match s with
  | T id -> id
  | NT id -> id

(* A nonterminal symbols is given by the following grammar:

   <nonterm>    ::= '<'<nonterm-id>'>'
   <nonterm-id> ::= <lower>{<lower>}{'-'<lower>{<lower>}}
   <lower>      ::= 'a' | 'b' | ... | 'y' | 'z'

   That is, a nonterminal symbol identifier is represented by a
   nonempty sequence of lowercase letters with intermediate dashes,
   but the dashes can not appear at the beginning, the end or adjacent
   to other dashes.  A nonterminal symbol is a nonterminal symbol
   identifier surrounded in angle brackets (no whitespace).

   The string given to the constructor `NT` should be an identifier.
   In paticular, it SHOULD NOT have angle brackets.

   For testing purposes you SHOULD NOT consume whitespace before or
   after a nonterminal symbol.

*)
let parse_nonterm : symbol parser = (* TODO *)
  let lower = many1 (satisfy is_lower_case) in
  let rest = map2 (fun x xs -> x :: xs) (char '-') lower in
  let cs = map2 (fun xs xss -> List.concat (xs :: xss)) lower (many rest) in
  char '<' >> cs << char '>' >|= fun cs -> NT (implode cs)

(* A terminal symbol is ANY sequence of characters between two single
   quotes, e.g.,

   + 'id'
   + ' '
   + '<>'
   + 'nn\n'

   For testing purposes you SHOULD NOT consume whitespace before or
   after terminal symbols.

*)
let parse_term : symbol parser = (* TODO *)
  (char '\'') >>
  (many (satisfy ((<>) '\'')) >|= implode) <<
  (char '\'') >|= fun x -> T x

(* `parse_symbol` parses either a terminal or a nonterminal symbol.

   For testing purposes you SHOULD NOT consume whitespace before or
   after symbols.
*)
let parse_symbol : symbol parser = (* TODO *)
  parse_term <|> parse_nonterm

(* A sentential form is given by the following grammar:

   <sentform> ::= <symbol> { <symbol> }
   <sentform> ::= 'EMPTY'

   where EMPTY is the empty string, and is represented abstractly as
   an empty list.

   For testing purposes you SHOULD NOT consume whitespace before a
   sentential form, but you SHOULD consume whitespace after a
   sentential form.

*)
let parse_sentform : sentform parser = (* TODO *)
  (many1 (parse_symbol << ws)) <|>
  (keyword "EMPTY" >| [])

(* An alternative is given by the following grammar:

   <alt> ::= <sentform> { '|' <sentform> }

   There can be any amount of whitespace around the alternative symbols '|'.

   For testing purposes you SHOULD NOT consume whitespace before an
   alternative, but you SHOULD consume whitespace after an
   alternative.
*)
let parse_alt : sentform list parser = (* TODO *)
  map2
    (fun x xs -> x :: xs)
    parse_sentform
    (many ((keyword "|") >> parse_sentform))

(* A rule is given by the following grammar:

   <rule> ::= <nonterm> '::=' <alt> '.'

   Note that `parse_rule` is a `rule list parser` since alternative
   notation is syntactic sugar for multiple rules.

   For testing purposes you SHOULD NOT consume whitespace before a
   rule, but you SHOULD consume whitespace after a rule.

*)
let parse_rule : rule list parser = (* TODO *)
  map2
    (fun nt sfs -> List.map (fun sf -> nt, sf) sfs)
    (parse_nonterm << ws << keyword "::=" >|= ident_of)
    (parse_alt << keyword ".")

(* `parse_grammar` should parse any number of rules (possibly zero).

   <grammar> ::= {<rule>}

   For testing purposes you SHOULD consume whitespace before and after
   a grammar.
*)
let parse_grammar : grammar parser = (* TODO *)
  ws >> many parse_rule >|= List.concat

(* TEST CASES *)

(* parse_term *)

let test = parse parse_term "'test'"
let out = Some (T "test")
let _ = assert (test = out)

let test = parse parse_term "'( '"
let out = Some (T "( ")
let _ = assert (test = out)

let test = parse parse_term "'<ok>'"
let out = Some (T "<ok>")
let _ = assert (test = out)

let test = parse parse_term "  'test'"
let out = None
let _ = assert (test = out)

let test = parse parse_term "'te"
let out = None
let _ = assert (test = out)

let test = parse parse_term "'-' "
let out = None
let _ = assert (test = out)

(* parse_nonterm *)

let test = parse parse_nonterm "<test>"
let out = Some (NT "test")
let _ = assert (test = out)

let test = parse parse_nonterm "<test-two-three>"
let out = Some (NT "test-two-three")
let _ = assert (test = out)

let test = parse parse_nonterm "<>"
let out = None
let _ = assert (test = out)

let test = parse parse_nonterm " <test>"
let out = None
let _ = assert (test = out)

let test = parse parse_nonterm "<test> "
let out = None
let _ = assert (test = out)

let test = parse parse_nonterm "< test>"
let out = None
let _ = assert (test = out)

let test = parse parse_nonterm "<test--two>"
let out = None
let _ = assert (test = out)

(* parse_symbol *)

let test = parse parse_symbol "'O^(*&'"
let out = Some (T "O^(*&")
let _ = assert (test = out)

let test = parse parse_symbol "test-two"
let out = None
let _ = assert (test = out)

let test = parse parse_symbol "<test-two>"
let out = Some (NT "test-two")
let _ = assert (test = out)

(* parse_sentform *)

let test = parse parse_sentform "  \n"
let out = None
let _ = assert (test = out)

let test = parse parse_sentform "EMPTY   "
let out = Some []
let _ = assert (test = out)

let test = parse parse_sentform "   EMPTY "
let out = None
let _ = assert (test = out)

let test = parse parse_sentform "<test>"
let out = Some [NT "test"]
let _ = assert (test = out)

let test = parse parse_sentform "<one><two> 'thr-ee' 'four' <fi-ve> '>' "
let out = Some [NT "one"; NT "two"; T "thr-ee"; T "four"; NT "fi-ve"; T ">"]
let _ = assert (test = out)

let test = parse parse_sentform " <one><two> 'thr-ee' 'four' <fi-ve> '>' "
let out = None
let _ = assert (test = out)

(* parse_alt *)

let test = parse parse_alt "EMPTY  "
let out = Some [[]]
let _ = assert (test = out)

let test = parse parse_alt "EMPTY | EMPTY | <one> <two> '3' "
let out = Some [[];[];[NT "one"; NT "two"; T "3"]]
let _ = assert (test = out)

let test = parse parse_alt "  EMPTY | EMPTY | <one> <two> '3' "
let out = None
let _ = assert (test = out)

let test = parse parse_alt "<factor> | <term> '*' <factor> | <term> '/' <factor>  "
let out = Some
    [ [NT "factor"]
    ; [NT "term"; T "*"; NT "factor"]
    ; [NT "term"; T "/"; NT "factor"]
    ]
let _ = assert (test = out)

(* parse rule *)

let test = parse parse_rule "<test> ::= ."
let out = None
let _ = assert (test = out)

let test = parse parse_rule "<test> ::= <test>.  "
let out = Some [("test", [NT "test"])]
let _ = assert (test = out)

let test = parse parse_rule "<test> ::= <test>  .  "
let out = Some [("test", [NT "test"])]
let _ = assert (test = out)

let test = parse parse_rule "  <test> ::= test."
let out = None
let _ = assert (test = out)

let test = parse parse_rule "<test> ::= <test> | <test> '***' ."
let out = Some [("test", [NT "test"]); ("test", [NT "test"; T "***"])]
let _ = assert (test = out)

let test = parse parse_rule
    "<term>       ::= <factor> | <term> '*' <factor> | <term> '/' <factor> .\n"
let out = Some
    [ ("term", [NT "factor"])
    ; ("term", [NT "term"; T "*"; NT "factor"])
    ; ("term", [NT "term"; T "/"; NT "factor"])
    ]
let _ = assert (test = out)

(* parse grammar *)

let test = parse parse_grammar "    \n   "
let out = Some []
let _ = assert (test = out)

let test = parse parse_grammar " <a>::=<a>.<b>::=<b>. "
let out = Some [("a", [NT "a"]); ("b", [NT "b"])]
let _ = assert (test = out)

let input = "
   <expr>       ::= <term> | <expr> '+' <term> | <expr> '-' <term> .
   <term>       ::= <factor> | <term> '*' <factor> | <term> '/' <factor> .
   <factor>     ::= <var-or-num> | '(' <expr> ')' .
   <var-or-num> ::= <var> | <digits> .
   <var>        ::= 'x' | 'y' | 'z' .
   <digits>     ::= <digit> | <digit> <digits> .
   <digit>      ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' .
"

let out =
  [ ("expr", [NT "term"])
  ; ("expr", [NT "expr"; T "+"; NT "term"])
  ; ("expr", [NT "expr"; T "-"; NT "term"])
  ; ("term", [NT "factor"])
  ; ("term", [NT "term"; T "*"; NT "factor"])
  ; ("term", [NT "term"; T "/"; NT "factor"])
  ; ("factor", [NT "var-or-num"])
  ; ("factor", [T "("; NT "expr"; T ")"])
  ; ("var-or-num", [NT "var"])
  ; ("var-or-num", [NT "digits"])
  ; ("var", [T "x"])
  ; ("var", [T "y"])
  ; ("var", [T "z"])
  ; ("digits", [NT "digit"])
  ; ("digits", [NT "digit"; NT "digits"])
  ; ("digit", [T "0"])
  ; ("digit", [T "1"])
  ; ("digit", [T "2"])
  ; ("digit", [T "3"])
  ; ("digit", [T "4"])
  ; ("digit", [T "5"])
  ; ("digit", [T "6"])
  ; ("digit", [T "7"])
  ; ("digit", [T "8"])
  ; ("digit", [T "9"])
  ]

let test = parse parse_grammar input
let out = Some out
let _ = assert (test = out)

(* END OF TEST CASES *)
