let rec fact n =
  match n with
  | 0 -> 1
  | n -> n * fact (n - 1)

let rec sum n =
  match n with
  | 0 -> 0
  | n -> n + sum (n - 1)

let rec upto f n start =
  let rec go n =
    match n with
    | 0 -> start
    | n -> f n (go (n - 1))
  in go n

let fact = upto ( * ) 0 1
let sum = upto ( + ) 0 0

let accum f l start =
  let rec go l =
    match l with
    | [] -> start
    | x :: xs -> f x (go xs)
  in go l

let rec range i j = if i > j then [] else i :: range (i + 1) j

let fact n = accum ( * ) (range 0 n) 1
let sum n = accum ( * ) (range 0 n) 0

let fact n = List.fold_right ( * ) (range 0 n) 1
let sum n = List.fold_right ( + ) (range 0 n) 0
let concat ls = List.fold_right ( @ ) ls []


