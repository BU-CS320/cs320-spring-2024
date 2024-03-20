(*
   Write an EBNF grammar for regular expressions for binary strings.

   Quantification has highest precedence (e.g., 01* is
   implicitly parenthesized as 0( 1* ) and 0|1* as 0|( 1* ).

   Grouping has second highest precedence, so 0|11 is 0|(11),
   Associativity does not matter.

   Alternatives have lowest precedence. Associativity does not
   matter.

   Example sentences:
   (01)*01*
   ((0))
   (0)1**
   ( 0|1|01|001* )01

   ======================================

   Write an ADT for these regular expressions.  Note that your ADT
   does not need to account for parentheses.
*)

(*
 * <regex> ::= <alt> { | <alt> }
 * <alt>   ::= <star> { <star> }
 * <star>  ::= <term> {*}
 * <term>  ::= ( <regex> ) | 0 | 1
 *)

type regex =
  | Zero
  | One
  | Alternative of regex * regex
  | Star of regex
  | Group of regex * regex

let ex1 = Group (Star (Group (Zero, One)), Group (Zero, Star One))
let ex2 = Zero
let ex3 = Group (Zero, Star (Star One))

let ex4 =
  Group
    ( Alternative
        ( Zero,
          Alternative
            ( One,
              Alternative
                (Group (Zero, One), Group (Zero, Group (Zero, Star One))) ) ),
      Group (Zero, One) )
