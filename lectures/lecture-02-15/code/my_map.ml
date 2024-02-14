let rec map f l =
  match l with
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec map_t f l =
  let rec go l acc =
    match l with
    | [] -> List.rev acc
    | x :: xs -> go xs (f x :: acc)
  in go l []