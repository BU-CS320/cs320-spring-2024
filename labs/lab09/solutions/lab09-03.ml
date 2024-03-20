(*
   TA/TFs + Myself: If there is time, go through a simple
   recursive-decent parser for the grammar given earlier (with
   simplifying assumptions, e.g., maximum parentheses, right
   associativity, etc.)
*)

(*
 * <regex> ::= <alt> { | <alt> }
 * <alt>   ::= <star> { <star> }
 * <star>  ::= <term> {*}
 * <term>  ::= ( <regex> ) | 0 | 1
 *)
#use "./lab09-01.ml"

#use "./lab09-02.ml"

let rec parseRegex (ts : token list) : (regex * token list) option =
  match parseAlt ts with
  | None -> None
  | Some (alt1, rest) -> (
      match rest with
      | TBar :: rest -> (
          match parseRegex rest with
          | None -> None
          | Some (alt2, rest) -> Some (Alternative (alt1, alt2), rest))
      | _ -> Some (alt1, rest))

and parseAlt (ts : token list) : (regex * token list) option =
  match parseStar ts with
  | None -> None
  | Some (star1, rest) -> (
      match parseAlt rest with
      | None -> Some (star1, rest)
      | Some (star2, rest) -> Some (Group (star1, star2), rest))

and parseStar (ts : token list) : (regex * token list) option =
  let rec starLoop reg ts =
    match ts with
    | TStar :: rest -> starLoop (Star reg) rest
    | _ -> reg, ts
  in
  match parseTerm ts with
  | None -> None
  | Some (term, rest) -> Some (starLoop term rest)

and parseTerm (ts : token list) : (regex * token list) option =
  match ts with
  | TZero :: rest -> Some (Zero, rest)
  | TOne :: rest -> Some (One, rest)
  | TLPar :: rest -> (
      match parseRegex rest with
      | None -> None
      | Some (reg, rest) -> (
          match rest with
          | TRPar :: rest -> Some (reg, rest)
          | _ -> None))
  | _ -> None

let parse (s : string) : regex option =
  match tokenize_s s with
  | None -> None
  | Some ts ->
    match parseRegex ts with
    | Some (reg, []) -> Some reg
    | _ -> None

let () = assert (parse "(01)*01*" = Some ex1)
let () = assert (parse "((0))" = Some ex2)
let () = assert (parse "(0)1**" = Some ex3)
let () = assert (parse "( 0|1|01|001* )01" = Some ex4)
