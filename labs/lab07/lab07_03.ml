(* Deepest Parentheses

   Implement the function `deepest_parens` which, given

     ps : paren list with a balanced set of parentheses

   returns the index of (the left parentheses of) the leftmost deepest
   pair of parentheses in `ps`.  You should use List.fold_left.

   Examples:
   let _ = assert (deepest_parens [Op;Cl] = 0)                       ()
   let _ = assert (deepest_parens [Op;Cl;Op;Op;Cl;Cl] = 3)           ()(())
   let _ = assert (deepest_parens [Op;Op;Cl;Op;Cl;Cl] = 1)           (()())
   let _ = assert (deepest_parens [Op;Op;Cl;Op;Op;Cl;Cl;Cl] = 4)     (()(()))

   Note to self/TFs/TAs: There are a number of ways to do this, using
   records is one way.

*)

type paren
  = Op
  | Cl

let deepest_parens (ps : paren list) : int =
  assert false (* TODO *)
