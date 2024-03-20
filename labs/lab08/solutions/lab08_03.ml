(* Get Token

   Implement the function `next` which, given

     s: char list

   returns a list of pairs of type `(t * char list)`, where a pair

     (x, rest)

   appears in the list if `s` starts with the same sequence of symbols used
   in the constructor `x`, and `rest` is the remaining letters.

   Examples:
   let _ = assert
     (next ['A';'B';'D'] = [(AB, ['D'])])
   let _ = assert
     (next ['A';'B';'C';'D'] = [(AB, ['C';'D']); (ABC, ['D'])])

*)

type t
  = AB
  | ABC
  | DE

let next (s : char list) : (t * char list) list =
  let check_AB =
    match s with
    | 'A' :: 'B' :: xs -> [(AB, xs)]
    | _ -> []
  in
  let check_ABC =
    match s with
    | 'A' :: 'B' :: 'C' :: xs -> (ABC, xs) :: check_AB
    | _ -> check_AB
  in
  let check_DE =
    match s with
    | 'D' :: 'E' :: xs -> (DE, xs) :: check_ABC
    | _ -> check_ABC
  in check_DE
