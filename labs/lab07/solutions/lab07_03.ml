(* Deepest Parentheses

   Implement the function `deepest_parens` which, given

     ps : paren list with a balanced set of parentheses

   returns the index of (the left parentheses of) the leftmost deepest
   pair of parentheses in `ps`.

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

type acc =
  { curr_depth : int
  ; max_depth : int
  ; best : int
  ; index : int
  }

let deepest_parens (ps : paren list) : int =
  let aux acc p =
    match p with
    | Op ->
      { acc with
        curr_depth = acc.curr_depth + 1 ;
        index = acc.index + 1 ;
      }
    | Cl ->
      let m = max acc.curr_depth acc.max_depth in
      { index = acc.index + 1 ;
        curr_depth = acc.curr_depth - 1 ;
        max_depth = m ;
        best = if m > acc.max_depth then acc.index - 1 else acc.best ;
      }
  in
  let init =
    { index = 0 ;
      curr_depth = 0 ;
      max_depth = 0 ;
      best = 0 ;
    }
  in (List.fold_left aux init ps).best

let _ = assert (deepest_parens [Op;Cl] = 0)
let _ = assert (deepest_parens [Op;Cl;Op;Op;Cl;Cl] = 3)
let _ = assert (deepest_parens [Op;Op;Cl;Op;Cl;Cl] = 1)
let _ = assert (deepest_parens [Op;Op;Cl;Op;Op;Cl;Cl;Cl] = 4)
