(* CS 320, Feb 8, 2024 *)

(* The following are equivalent ways of defining the function foo:
     let foo p1 p2 ... pn = expr
     let foo = function p1 -> function p2 -> ... -> function pn -> expr
     let foo = fun p1 p2 ... pn -> expr
*)

let head : 'a list -> 'a option = function
  | [] -> None
  | h :: t -> Some h
let tail : 'a list -> 'a list option = function
  | [] -> None
  | h :: t -> Some t

type 'a matrix = 'a list list

let rec first_column : 'a matrix -> 'a matrix option = function
  | [] -> None
  | h :: t -> match h with
              | [] -> None 
              | hh :: tt -> match first_column t with
                            | None -> Some [[hh]]
                            | Some ttt -> Some ([hh] :: ttt)

let rec rest_columns : 'a matrix -> 'a matrix option = function
  | [] -> None
  | h :: t -> match h with
              | [] -> None
              | hh :: tt -> match rest_columns t with
                            | None -> Some [tt]
                            | Some ttt -> Some (tt :: ttt)

(* the transpose of matrix
   | 1 2 3 |
   | 4 5 6 |
is the matrix
   | 1 4 |
   | 2 5 |
   | 3 6 |
*)

(* the diagonal in a full, not truncated, square matrix, e.g.
   | 1 2 3 |
   | 4 5 6 |
   | 7 8 9 |
is [ 1 ; 5 ; 9 ]  and its counter-diagonal is [ 7 ; 5 ; 3 ].
*)   

   
