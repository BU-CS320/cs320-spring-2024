(* Sorting Tuples 

   Given a tuple of integers of size 5, sort all of its elements
   WITHOUT first converting it into a list or array. *)

type tup2 = int * int
type tup3 = int * int * int
type tup4 = int * int * int * int
type tup5 = int * int * int * int * int

let sort (xs : tup5): tup5 = 
  assert false

let sort2 (xs: tup2): tup2 =
  let (a, b) = xs in
  if a <= b then (a, b) else (b, a)

let sort3 (xs: tup3): tup3 =
  let (a, b, c) = xs in
  let (b, c) = sort2 (b, c) in
  if a <= b then
    (a, b, c)
  else
    let (a, c) = sort2 (a, c) in
    (b, a, c)

let sort4 (xs : tup4): tup4 =
  let (a, b, c, d) = xs in
  let (b, c, d) = sort3 (b, c, d) in
  if a <= b then
    (a, b, c, d)
  else
    let (a, c, d) = sort3 (a, c, d) in
    (b, a, c, d)

let sort (xs : tup5): tup5 = 
  let (a, b, c, d, e) = xs in
  let (b, c, d, e) = sort4 (b, c, d, e) in
  if a <= b then
    (a, b, c, d, e)
  else
    let (a, c, d, e) = sort4 (a, c, d, e) in
    (b, a, c, e, e)