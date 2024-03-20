(* Derivations

   <expr> ::= <expr> + <term>
            | <term>
   <term> ::= <term> * <var>
            | <pars>
   <pars> ::= <var> | ( <expr> )
   <var>  ::= x | y | z

   Give a derivation of `( x + y ) * z` in the above grammar.
*)

(*
   <expr>
   <term>
   <term> * <var>
   <pars> * z
   ( <expr> ) * z
   ( <expr> + <term> ) * z
   ( <term> + <term> ) * z
   ( <pars> + <pars> ) * z
   ( <var> + <var> ) * z
   ( x + y ) * z
*)
