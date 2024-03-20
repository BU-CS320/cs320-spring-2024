(* Boolean Expressions

   Write a grammar for boolean expression in Python with
   the terminal symbols `and`, `or`, `not`, `True`, and `False`.
   Experiment to determine the precedence of operators (since no
   operators have the same precedence, associativity is not an issue,
   but you can take them to be left-associative).
*)

(*
   <expr> ::= <expr> or <term>
            | <term>
   <term> ::= <term> and <neg>
            | <neg>
   <neg>  ::= not <bool>
            | <bool>
   <bool> ::= True | False
*)
