type 'a concatlist =
  | Empty
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let map = assert false (* TODO *)
let fold_left = assert false (* TODO *)








































(* A SOLUTION *)
let rec map f l =
  match l with
  | Empty -> Empty
  | Single x -> Single (f x)
  | Concat (l, r) -> Concat (map f l, map f r)

let fold_left op base l =
  let rec go l curr =
    match l with
    | Empty -> curr
    | Single x -> op curr x
    | Concat (l, r) -> go r (go l curr)
  in go l base

let l = Concat (Concat (Single 1, Single 2), Single 3)
let _ = assert (fold_left (-) 0 l = (-6))


