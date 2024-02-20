let fold_left op base l =
  let rec go l acc =
    match l with
    | [] -> acc
    | x :: xs -> go xs (op acc x)
  in go l base