type 'a tree =
  | Leaf
  | Node of 'a * 'a tree *'a tree

let map f t =
  let rec go t =
    match t with
    | Leaf -> Leaf
    | Node (x, l, r) -> Node (f x, go l, go r)
  in go t

let fold_left op base t =
  let rec go acc t=
    match t with
    | Leaf -> acc
    | Node (x, l, r) -> go (op (go acc l) x) r
  in go base t

let rec rev t =
  match t with
  | Leaf -> Leaf
  | Node (x, l, r) -> Node (x, rev r, rev l)

let fold_right op t base =
  fold_left (fun x y -> op y x) base (rev t)

let t =
  Node(2,
    Node(1, Leaf, Leaf),
    Node(3, Leaf, Leaf))

let inorder t = fold_right (fun x xs -> x :: xs) t []

