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

let insert (x : 'a) (t : 'a tree) : 'a tree = assert false (* TODO *)

let flatten (t : 'a tree) : 'a list = assert false (* TODO *)

let sort (l : 'a list) : 'a list = assert false (* TODO *)
