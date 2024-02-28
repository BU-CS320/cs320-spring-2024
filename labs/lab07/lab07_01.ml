(* Consecutives

   1. Implement the function `map2` which, given

     f : 'a -> 'b -> 'c
     l : 'a list
     r : 'b list

   returns the result of applying `f` to each element of `l` and `r`
   pointwise.  If `l` and `r` are different lengths, then the longer
   list should be truncated to the length of the shorter list.

   For example, `map2 f [a;b] [c;d;e]` is equivalent to

     [f a c ; f b d]

   This is a two-list version of the function `List.map`.  You should
   not use the function `List.map2` in your implementation (the
   standard library version does not work the way we've defined `map2`
   here).

   ====================

   2. Implement the function `consecutives` which, given

     len : a positive integer
     l : a list

   returns the list of all consecutive sublists of `l` of length

     min len (List.length l)

   Example:
   let _ = assert (consecutives 2 [1;2;3;4;5] = [[1;2];[2;3];[3;4];[4;5]])
   let _ = assert (consecutives 1 [] = [[]])
   let _ = assert (consecutives 10 [1;2;3;4;5] = [[1;2;3;4;5]])

   Hint: Use the functions `map2` and `List.map`.

*)

let rec map2 (f : 'a -> 'b -> 'c) (l : 'a list) (r : 'b list) : 'c list =
  assert false (* TODO *)

let consecutives (len : int) (l : 'a list) : 'a list list =
  assert false (* TODO *)
