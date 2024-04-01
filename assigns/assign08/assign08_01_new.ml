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

(* Parsing BNF Specifications (Again) (Again)

   In this problem, you will be using parser combinators to build a(n
   even) better parser for BNF specifications.  In particular, it
   should handle:

   + any sequence of symbols between single quotes as terminal symbols
   + names with dashes in nonterminal symbols
   + alternatives in rules
   + nonterminals, terminals, and symbols '::=' and 'EMPTY' should not
   contain whitespace, but the parser should be whitespace agnostic
   otherwise

   + NEW: (non-recursive) repetitions and optionals in rules
   + NEW: multiple grammars in a single file
   + NEW: references to nonterminal symbols in other grammars
   + NEW: rule-declaration keyword

   An example of something that should be successfully parsed:

   BEGIN FLOAT
     RULE <float> ::= [ '-' ] <digit> { <digit> } [ '.' <digit>]
     RULE <digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
   END

   BEGIN EXPRESSION
     RULE <expr>       ::= <term> { '+' <term> | '-' <term> }
     RULE <term>       ::= <factor> { '*' <factor> | '/' <factor> }
     RULE <factor>     ::= <var> | FLOAT.<float> | '(' <expr> ')'
     RULE <var>        ::= <letter> { <letter> | FLOAT.<digit> }
     RULE <letter>     ::=
       'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' |
       'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' |
       'o' | 'p' | 'q' | 'r' | 's' | 't' |
       'u' | 'v' | 'w' | 'x' | 'y' | 'z'
   END

*)

type nt_ident = string (* identifier for nonterminal symbols *)
type g_ident = string (* identifier for grammars *)
type symbol
  = T of string
  | NT of nt_ident
  | NTRef of g_ident * nt_ident (* reference to nonterminal symbol in a grammar *)
type symbol_complex
  = Sym of symbol
  | Opt of symbol list list
  | Rep of symbol list list
type sentform = symbol_complex list
type rule = nt_ident * sentform
type grammar = g_ident * rule list
type grammars = grammar list

(* A grammar identifier is given by the following grammar:

   <g-ident> ::= <upper>{<upper>}
   <upper>   ::= 'A' | 'B' | 'C' | ... | 'Y' | 'Z'

   For testing purposes, you SHOULD NOT consume whitespace before or
   after a grammar identifier.
*)
let parse_g_ident : string parser = (* TODO *)
  assert false

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
  assert false

(* A nonterminal symbols is given by the following grammar:

   <nonterm>    ::= [<g-ident>'.']'<'<nonterm-id>'>'
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
  assert false

(* `parse_symbol` parses either a terminal, a nonterminal symbol or a
   nonterminal reference.

   <symbol> ::= <term> | <nonterm>

   For testing purposes you SHOULD NOT consume whitespace before or
   after symbols.

*)
let parse_symbol : symbol parser = (* TODO *)
  assert false

(* A complex symbol is given by the following grammar:

   <symbols>         ::= <symbol> { <symbol> } | EMPTY
   <alt-symbol-list> ::= <symbol> { '|' <symbols> }
   <symbol-complex> ::= '{' <alt-symbol-list> '}'
                      | '[' <alt-symbol-list> '}'
                      | <symbol>

   Note that this implies there cannot be a repetitions or optionals
   within other repetitions or optionals.

   For testing purposes, you SHOULD NOT consume whitespace before, but
   SHOULD consume whitespace after.

*)
let parse_symbol_complex : symbol_complex parser =
  assert false

(* A sentential form is given by the following grammar:

   <sentform> ::= <symbol-complex> { <symbol-complex> } | EMPTY

   where EMPTY is the empty string, and is represented abstractly as
   an empty list.

   For testing purposes you SHOULD NOT consume whitespace before a
   sentential form, but you SHOULD consume whitespace after a
   sentential form.

*)
let parse_sentform : sentform parser = (* TODO *)
  assert false

(* A rule is given by the following grammar:

   <alt>  ::= <sentform> { '|' <sentform> }
   <rule> ::= 'RULE' <nonterm> '::=' <alt>

   Note that `parse_rule` is a `rule list parser` since alternative
   notation is syntactic sugar for multiple rules.

   For testing purposes you SHOULD NOT consume whitespace before a
   rule, but you SHOULD consume whitespace after a rule.

*)
let parse_rule : rule list parser = (* TODO *)
  assert false

(* A grammar is given by the following grammar:

   <grammar> ::= 'BEGIN' <g-ident> { <rule> } 'END'

   For testing purposes you SHOULD NOT consume whitespace before a
   grammar, but you SHOULD consume whitespace and after a grammar.
*)
let parse_grammar : grammar parser = (* TODO *)
  assert false

(* A collection grammars is given by the following grammar:

   <grammars> ::= { <grammar> }

   For testing purposes, you should consume whitespace before and
   after a collection of grammars.

*)
let parse_grammars : grammars parser = (* TODO *)
  assert false

(* UNCOMMENT AS YOU COMPLETE THE ASSIGNMENT
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

let test = parse parse_symbol "TEST.<test-two>"
let out = Some (NTRef ("TEST", "test-two"))
let _ = assert (test = out)

(* parse_symbol_complex *)

let test = parse parse_symbol_complex "TEST.<test-two>"
let out = Some (Sym (NTRef ("TEST", "test-two")))
let _ = assert (test = out)

let test = parse parse_symbol_complex "{ <one> <two> | 'three' }"
let out = Some (Rep [[NT "one"; NT "two"]; [T "three"]])
let _ = assert (test = out)

let test = parse parse_symbol_complex "{ <one> <two> | 'three' | EMPTY }"
let out = Some (Rep [[NT "one"; NT "two"]; [T "three"]; []])

let test = parse parse_symbol_complex "[ <one> <two> | 'three' ]"
let out = Some (Opt [[NT "one"; NT "two"]; [T "three"]])
let _ = assert (test = out)

let test = parse parse_symbol_complex "[ <one> <two> | 'three' | EMPTY ]"
let out = Some (Opt [[NT "one"; NT "two"]; [T "three"]; []])

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
let out = Some [Sym (NT "test")]
let _ = assert (test = out)

let test = parse parse_sentform "<one><two> 'thr-ee' 'four' <fi-ve> '>' "
let out = Some [Sym (NT "one"); Sym (NT "two"); Sym (T "thr-ee"); Sym (T "four"); Sym (NT "fi-ve"); Sym (T ">")]
let _ = assert (test = out)

let test = parse parse_sentform " <one><two> 'thr-ee' 'four' <fi-ve> '>' "
let out = None
let _ = assert (test = out)

let test = parse parse_sentform "<one> <two> { <three> | <four> } [ <five> 'six' ]"
let out = Some [Sym (NT "one"); Sym (NT "two"); Rep [[NT "three"]; [NT "four"]]; Opt [[NT "five"; T "six"]]]
let _ = assert (test = out)

let test = parse parse_sentform "<factor> { '*' <factor> | '/' <factor> }  "
let out = Some [Sym (NT "factor"); Rep [[T "*"; NT "factor"]; [T "/"; NT "factor"]]]
let _ = assert (test = out)

(* parse_rule *)

let test = parse parse_rule "RULE <test> ::=  "
let out = None
let _ = assert (test = out)

let test = parse parse_rule "RULE <test> ::= <test>  "
let out = Some [("test", [Sym (NT "test")])]
let _ = assert (test = out)

let test = parse parse_rule "  RULE <test> ::= test"
let out = None
let _ = assert (test = out)

let test = parse parse_rule "RULE<test> ::= <test> | <test> '***' "
let out = Some [("test", [Sym (NT "test")]); ("test", [Sym (NT "test"); Sym (T "***")])]
let _ = assert (test = out)

let test = parse parse_rule
    "RULE <term>       ::= <factor> | <term> '*' <factor> | <term> '/' <factor> \n"
let out = Some
    [ ("term", [Sym (NT "factor")])
    ; ("term", [Sym (NT "term"); Sym (T "*"); Sym(NT "factor")])
    ; ("term", [Sym (NT "term"); Sym (T "/"); Sym(NT "factor")])
    ]
let _ = assert (test = out)

let test = parse parse_rule "RULE <term> ::= <factor> { '*' <factor> | '/' <factor> }  "
let out = Some ["term", [Sym (NT "factor"); Rep [[T "*"; NT "factor"]; [T "/"; NT "factor"]]]]
let _ = assert (test = out)

(* parse grammar *)

let test = parse parse_grammar "    \n   "
let out = None
let _ = assert (test = out)

let test = parse parse_grammar "BEGIN GRAMMAR END"
let out = Some ("GRAMMAR", [])
let _ = assert (test = out)

let test = parse parse_grammar "BEGIN OTHER RULE <a>::=<a> RULE<b>::=<b> END "
let out = Some ("OTHER", [("a", [Sym(NT "a")]); ("b", [Sym (NT "b")])])
let _ = assert (test = out)

let test = parse parse_grammar "BEGIN EXPR
    RULE <expr>       ::= <term> | <expr> '+' <term> | <expr> '-' <term>
    RULE <term>       ::= <factor> | <term> '*' <factor> | <term> '/' <factor>
    RULE <factor>     ::= <var-or-num> | '(' <expr> ')'
    RULE <var-or-num> ::= <var> | <digits>
    RULE <var>        ::= 'x' | 'y' | 'z'
    RULE <digits>     ::= <digit> | <digit> <digits>
    RULE <digit>      ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
  END
"
let out = Some
  ( "EXPR",
  [ ("expr", [Sym (NT "term")])
  ; ("expr", [Sym (NT "expr"); Sym (T "+"); Sym (NT "term")])
  ; ("expr", [Sym (NT "expr"); Sym (T "-"); Sym (NT "term")])
  ; ("term", [Sym (NT "factor")])
  ; ("term", [Sym (NT "term"); Sym (T "*"); Sym (NT "factor")])
  ; ("term", [Sym (NT "term"); Sym (T "/"); Sym (NT "factor")])
  ; ("factor", [Sym (NT "var-or-num")])
  ; ("factor", [Sym (T "("); Sym (NT "expr"); Sym (T ")")])
  ; ("var-or-num", [Sym (NT "var")])
  ; ("var-or-num", [Sym (NT "digits")])
  ; ("var", [Sym (T "x")])
  ; ("var", [Sym (T "y")])
  ; ("var", [Sym (T "z")])
  ; ("digits", [Sym (NT "digit")])
  ; ("digits", [Sym (NT "digit"); Sym (NT "digits")])
  ; ("digit", [Sym (T "0")])
  ; ("digit", [Sym (T "1")])
  ; ("digit", [Sym (T "2")])
  ; ("digit", [Sym (T "3")])
  ; ("digit", [Sym (T "4")])
  ; ("digit", [Sym (T "5")])
  ; ("digit", [Sym (T "6")])
  ; ("digit", [Sym (T "7")])
  ; ("digit", [Sym (T "8")])
  ; ("digit", [Sym (T "9")])
  ]
  )
let _ = assert (test = out)

(* parse_grammars *)

let test = parse parse_grammars "
   BEGIN FLOAT
     RULE <float> ::= [ '-' ] <digit> { <digit> } [ '.' <digit>]
     RULE <digit> ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
   END

   BEGIN EXPRESSION
     RULE <expr>       ::= <term> { '+' <term> | '-' <term> }
     RULE <term>       ::= <factor> { '*' <factor> | '/' <factor> }
     RULE <factor>     ::= <var> | FLOAT.<float> | '(' <expr> ')'
     RULE <var>        ::= <letter> { <letter> | FLOAT.<digit> }
     RULE <letter>     ::=
       'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' |
       'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' |
       'o' | 'p' | 'q' | 'r' | 's' | 't' |
       'u' | 'v' | 'w' | 'x' | 'y' | 'z'
   END
"

let out = Some
    [ ( "FLOAT" ,
        [ "float" , [Opt [[T "-"]]; Sym (NT "digit"); Rep [[NT "digit"]]; Opt [[T "." ; NT "digit"]]]
        ; "digit" , [Sym (T "0")]
        ; "digit" , [Sym (T "1")]
        ; "digit" , [Sym (T "2")]
        ; "digit" , [Sym (T "3")]
        ; "digit" , [Sym (T "4")]
        ; "digit" , [Sym (T "5")]
        ; "digit" , [Sym (T "6")]
        ; "digit" , [Sym (T "7")]
        ; "digit" , [Sym (T "8")]
        ; "digit" , [Sym (T "9")]
        ]
      )
    ; ( "EXPRESSION" ,
        [ "expr" , [Sym (NT "term"); Rep [[T "+"; NT "term"]; [T "-"; NT "term"]]]
        ; "term" , [Sym (NT "factor"); Rep [[T "*"; NT "factor"]; [T "/"; NT "factor"]]]
        ; "factor" , [Sym (NT "var")]
        ; "factor" , [Sym (NTRef ("FLOAT", "float"))]
        ; "factor" , [Sym (T "("); Sym (NT "expr"); Sym (T ")")]
        ; "var" , [Sym (NT "letter"); Rep [[NT "letter"]; [NTRef ("FLOAT", "digit")]]]
        ; "letter" , [Sym (T "a")]
        ; "letter" , [Sym (T "b")]
        ; "letter" , [Sym (T "c")]
        ; "letter" , [Sym (T "d")]
        ; "letter" , [Sym (T "e")]
        ; "letter" , [Sym (T "f")]
        ; "letter" , [Sym (T "g")]
        ; "letter" , [Sym (T "h")]
        ; "letter" , [Sym (T "i")]
        ; "letter" , [Sym (T "j")]
        ; "letter" , [Sym (T "k")]
        ; "letter" , [Sym (T "l")]
        ; "letter" , [Sym (T "m")]
        ; "letter" , [Sym (T "n")]
        ; "letter" , [Sym (T "o")]
        ; "letter" , [Sym (T "p")]
        ; "letter" , [Sym (T "q")]
        ; "letter" , [Sym (T "r")]
        ; "letter" , [Sym (T "s")]
        ; "letter" , [Sym (T "t")]
        ; "letter" , [Sym (T "u")]
        ; "letter" , [Sym (T "v")]
        ; "letter" , [Sym (T "w")]
        ; "letter" , [Sym (T "x")]
        ; "letter" , [Sym (T "y")]
        ; "letter" , [Sym (T "z")]
        ]
      )
    ]
let _ = assert (test = out)

(* END OF TEST CASES *)
*)
