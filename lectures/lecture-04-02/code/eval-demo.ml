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

let rec_parser (p : unit -> 'a parser) : 'a parser =
  pure () >>= p

(* END OF UTILITIES *)

type expr
  = Add of expr * expr
  | Eq of expr * expr
  | Int of int
  | Bool of bool
  | Error
