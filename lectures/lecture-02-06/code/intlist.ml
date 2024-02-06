
type intlist
  = Nil
  | Cons of int * intlist

let example = Cons (1, Cons (2, Cons (3, Nil)))

let rec snoc (xs : intlist) (x : int) : intlist =
  match xs with
  | Nil -> Cons (x, Nil)
  | Cons (y, ys) -> Cons (y, snoc ys x)

let _ = assert (snoc example 4 = Cons (1, Cons (2, Cons (3, Cons (4, Nil)))))