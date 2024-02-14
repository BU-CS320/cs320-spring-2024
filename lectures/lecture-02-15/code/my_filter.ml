let rec filter p l =
  match l with
  | [] -> []
  | x :: xs ->
    (if p x then [x] else []) @ filter p xs

let filter_tail p =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | x :: xs -> go ((if p x then [x] else []) @ acc) xs
  in go []