(* For Loops

   In Python we might write the following code to generate Pythagorean
   triples.

     def pythagoraean_triples(n):
         out = []
         for i in range(1, n):
             for j in range(i + 1, n):
                 for k in range(j + 1, n):
                     if i * i + j * j == k * k:
                         out.append((i, j, k))
         return out

   In this problem we will be implementing a version of this logic in
   OCaml using higher-order functions.  This problem has two parts:

   ====================

   1. Implement a function `for_loop` which, given

     l : 'a list
     f : a function of type ('a -> 'b list)

   returns the result of applying `f` to each element of the `l` and
   then concatenating the outputs.  For example, `for_loop [x;y;z] f`
   is equivalent to `f x @ f y @ f z`.

   Hint: Take a look at the `List.concat` in the standard library.

   ====================

   2. Implement a function `pythagorean_triples` which, given

     n : an integer

   returns the list of all triples (i, j, k) in lexicographical order
   such that

   - i * i + j * j = k * k
   - i < j < k < n

   You should use the `for_loop` function and mimic the structure of
   the above Python code.  See the example `test` below for how to use
   the `for_loop` function like a for-loop in Python.

   Think about how you might use `List.filter` to get the Pythagorean
   triples from the the list of all triples.

   Example:
   let _ = assert (pythagorean_triples 20 =
     [(3, 4, 5); (5, 12, 13); (6, 8, 10); (8, 15, 17); (9, 12, 15)])
*)

let rec range i j =
  if i >= j then
    []
  else
    i :: range (i + 1) j

let for_loop (l : 'a list) (f : 'a -> 'b list) : 'b list =
  assert false (* TODO *)

let foo i j =
  for_loop (range i j) (fun k ->
      [k + k])

(*
   foo is like the Python code:

   out = []
   for k in range(i, j):
       out.append(k + k)
   return out
*)

(* let _ = assert (foo 1 10 = List.map (fun k -> k + k) (range 1 10)) *)

let pythagorean_triples (n : int) : (int * int * int) list =
  assert false (* TODO *)
