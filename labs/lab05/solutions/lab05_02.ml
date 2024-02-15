(* Ordered Binary Trees

   Note to TF/TAs and myself: Feel free to include anything else about
   trees which you feel is important.

   A binary tree is ORDERED if it has the following property: if `t`
   is of the from `Node (a, left, right)`, then every element in
   `left` is at most `a` and every element in `right` is greater than
   `a`.  Repetitions are allowed.

   This problem has three parts:

   1. Implement the function `insert` which, given

     x: an element of type 'a
     t: ordered 'a tree

   returns an ordered tree with the same elements as `t` along with
   the elements in `t` along with the new element `x`.

   2. Implement the function `flatten` which, given

     t: ordered 'a tree

   returns an list with the same elements as `t` in sorted order.

   3. Implement a sorting function on lists using these two functions.

*)

type 'a tree
  = Empty
  | Node of 'a * 'a tree * 'a tree

let rec insert x t =
  match t with
  | Empty -> Node (x, Empty, Empty)
  | Node (a, left, right) ->
    if x <= a then
      Node (a, insert x left, right)
    else
      Node (a, left, insert x right)

let rec flatten t =
  match t with
  | Empty -> []
  | Node(x, left, right) -> (flatten left) @ [x] @ (flatten right)

let sort l =
  let rec build_tree l =
    match l with
    | [] -> Empty
    | x :: xs -> insert x (build_tree xs)
  in flatten (build_tree l)

let _ = assert(sort [3;1;4;2;-2;1;1;-3] = [-3;-2;1;1;1;2;3;4])
