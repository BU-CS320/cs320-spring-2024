(*
let rec insert (x : 'a) (l : 'a list) : 'a list =
  match l with
  | [] -> [x]
  | y :: ys ->
    if x <= y
    then x :: y :: ys
    else y :: insert x ys

let rec sort (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs -> insert x (sort xs)
*)

let insert (le : 'a -> 'a -> bool) =
  let rec go x l =
    match l with
    | [] -> [x]
    | y :: ys ->
      if le x y then
        x :: y :: ys
      else
        y :: go x ys
  in go

let rec sort (le : 'a -> 'a -> bool)  =
  let rec go l =
    match l with
    | [] -> []
    | x :: xs -> insert le x (go xs)
  in go