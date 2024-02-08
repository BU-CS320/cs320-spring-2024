(* Forklist

   A `forklist` is combination of a list and a binary tree.  It has
   constructors for the empty list (Nil), a single link (Cons) and a
   double link (Fork).

   A forklist `l` with DISTINCT elements is ORDERED if it satisfies the
   following properties:

   1. If `l` is of the form `Cons (x, xs)` then every element in `xs` is
   greater than `x`.

   2. If `l` is of the form `Fork (x, lxs rxs)` then every element in
   `lxs` is less than x and every element in `rxs` is greater than
   `x`.

   A forklist `l` is TAILED if it satisfies the property that if `Cons
   (x, xs)` appears in `l`, then `xs` contains no `Fork`s.

   Implement a function `delay_cons` which given

     f : an ordered forklist of integers

   returns a TAILED ordered forklist with the following properties:

   1. It has the same elements as `f`

   2. It has the same number of `Cons`s, `Fork`s and `Nil`s as `f`.

   Example:
   let f = Cons (2, Fork(4, Cons(3, Nil), Cons (5, Nil)))
   let g = Fork (4, Cons (2, Cons (3, Nil)), Cons(5, Nil))
   let _ = assert (delay_cons f = g)

   NOTE: the output does not need to look exactly like this. It just
   has to satisfy the above properties.

*)

type 'a forklist
  = Nil
  | Cons of 'a * 'a forklist
  | Fork of 'a * 'a forklist * 'a forklist

let delay_cons (f : int forklist) : int forklist =
  assert false (* TODO *)
