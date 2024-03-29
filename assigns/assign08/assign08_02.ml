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

(* Reverse Polish Notation (RPN) calculator REPL

   In this problem, you will be filling in a rudimentary
   read-eval-print-loop for the toy stack-based language we saw in
   lecture.  This means, rather than getting a program as a sequence
   of commands, we get a command one at a time and evaluate it on the
   fly.

   A command is represented by the ADT below.

   Once you've finished, you can uncomment the last line below and run
   the program to use the calculator.

*)

type command
  = Quit
  | Push of float
  | Pop
  | Add
  | Sub
  | Mul
  | Div

(* Parsing floats

   Implement `parse_float` which is a parser for floating point
   numbers using the following grammar:

   <float> ::= [-]<digit>{<digit>}[.<digit>]
   <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

   In particular, you may assume that there is at least one digits to
   the left of the decimal point (if the decimal point exists) and
   that there is no whitespace in a float.

   For testing purposes, you SHOULD NOT consume whitespace before or after
   a float.

*)
let parse_float : float parser = (* TODO *)
  let mk_float is_neg n odec =
    let cs =
      match odec with
      | None -> n
      | Some cs -> n @ ('.' :: cs)
    in
    let cs =
      match is_neg with
      | None -> cs
      | _ -> '-' :: cs
    in float_of_string (implode cs)
  in
  map3
    mk_float
    (optional (char '-'))
    (many1 (satisfy is_digit))
    (optional (char '.' >> many (satisfy is_digit)))

(* TEST CASES *)

let test = parse parse_float "0"
let out = Some 0.
let _ = assert (test = out)

let test = parse parse_float "-00.001"
let out = Some (-00.001)
let _ = assert (test = out)

let test = parse parse_float " 0.0"
let out = None
let _ = assert (test = out)

let test = parse parse_float "0.0 "
let out = None
let _ = assert (test = out)

let test = parse parse_float "123."
let out = Some 123.
let _ = assert (test = out)

let test = parse parse_float "123.4567"
let out = Some 123.4567
let _ = assert (test = out)

(* END OF TEST CASES *)

(* Parsing commmands

   Implement `parse_command`, a parser for commands using the
   following grammar:

   <command>
     ::= 'add'
       | 'sub'
       | 'mul'
       | 'div'
       | 'pop'
       | 'quit'
       | <float>

   where a float represents a Push command.

   For testing purposes you SHOULD consume whitespace before and
   after a command.

*)
let parse_command : command parser = (* TODO *)
  let p =
    (keyword "add" >| Add) <|>
    (keyword "sub" >| Sub) <|>
    (keyword "mul" >| Mul) <|>
    (keyword "div" >| Div) <|>
    (keyword "pop" >| Pop) <|>
    (keyword "quit" >| Quit) <|>
    (parse_float << ws >|= fun x -> Push x)
  in ws >> p

(* TEST CASES *)

let test = parse parse_command "  add "
let out = Some Add
let _ = assert (test = out)

let test = parse parse_command "sub"
let out = Some Sub
let _ = assert (test = out)

let test = parse parse_command "   -123.234  "
let out = Some (Push (-123.234))
let _ = assert (test = out)

let test = parse parse_command ""
let out = None
let _ = assert (test = out)

let test = parse parse_command "  "
let out = None
let _ = assert (test = out)

let test = parse parse_command " add quit "
let out = None
let _ = assert (test = out)

(* END OF TEST CASES *)

(* Evaluating commands

   Implement `run_command` which, given

   + cmd : command
   + stk : float list (* representing a stack *)

   determines the value of the stack after running the command.  You
   should use the evaluation rules from lecture, e.g., of the form

   (Div, x :: y :: stk) -> (x /. y) :: stk

   One deviation from these rules is that you will not handle errors.
   In the case that an operation cannot be performed the stack remains
   unchanged, e.g.

   (Pop, []) -> []
   (Add, [2.34]) -> [2.34]

   You may return anything on the Quit command.

*)
let run_command (cmd : command) (stk : float list) : float list = (* TODO *)
  match cmd, stk with
  | Quit, _ -> []
  | Push x, stk -> x :: stk
  | Pop, x :: rest -> rest
  | Add, x :: y :: rest -> x +. y :: rest
  | Sub, x :: y :: rest -> x -. y :: rest
  | Mul, x :: y :: rest -> x *. y :: rest
  | Div, x :: y :: rest -> x /. y :: rest
  | _ -> stk

(* TEST CASES *)

let test = run_command Add []
let out = []
let _ = assert (test = out)

let test = run_command Sub [2.; 3.; 5.; 6.]
let out = [-1.; 5.; 6.]
let _ = assert (test = out)

let test = run_command Pop [2.; 3.; 5.; 6.]
let out = [3.; 5.; 6.]
let _ = assert (test = out)

let test = run_command (Push 3.001) [2.; 3.; 5.; 6.]
let out = [3.001; 2.; 3.; 5.; 6.]
let _ = assert (test = out)

(* END OF TEST CASES *)

let rec print_stack (stk : float list) : unit =
  let rec go stk =
    match stk with
    | [] -> ()
    | x :: xs ->
      let _ = print_string "  " ; print_endline (string_of_float x) in
      go xs
  in
  print_endline "\n========" ;
  go stk ;
  print_endline "========\n"

let rec repl stk : unit =
  let continue stk = print_stack stk ; repl stk in
  let input = print_string "RPN> " ; read_line () in
  match parse parse_command input with
  | Some Quit -> print_endline "Goodbye."
  | Some cmd -> continue (run_command cmd stk)
  | _ ->
    print_endline "Could not parse. Try again." ;
    continue stk

(* let main = repl [] *)
