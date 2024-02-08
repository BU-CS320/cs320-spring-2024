(* CS 320, Feb 8, 2024 *)

type 'a mylist = 
  | Nil
  | Cons of 'a * 'a mylist
type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let l1 : int mylist = Cons (1, Cons (2, Cons (3, Nil)))
let l2 : int mylist = Cons (4, Cons (5, Cons (6, Nil)))

let t1 : int tree = Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7))

let rec myappend (l1 : 'a mylist) (l2 : 'a mylist) : 'a mylist =
  match l1 with
  | Nil -> l2
  | Cons (hd, tl) -> Cons (hd, myappend tl l2)

let rec preorder (t : 'a tree) : 'a mylist =
  match t with
  | Leaf x -> Cons (x, Nil)
  | Node (x,lft,rht) -> Cons (x, myappend (preorder lft) (preorder rht))

let rec postorder (t : 'a tree) : 'a mylist =
  match t with
  | Leaf x -> Cons (x, Nil)
  | Node (x,lft,rht) ->  myappend (myappend (postorder lft) (postorder rht)) (Cons(x,Nil))

let rec inorder (t : 'a tree) : 'a mylist =
  match t with
  | Leaf x -> Cons (x, Nil)
  | Node (x,lft,rht) ->  myappend (inorder lft) (Cons (x, inorder rht))



