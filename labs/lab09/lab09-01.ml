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

type regex = Unit (* TODO *)
