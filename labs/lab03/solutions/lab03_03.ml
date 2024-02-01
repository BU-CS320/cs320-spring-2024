(* Pythagorean Triples

   Implement the function `py_trip_hyp` of type `int -> bool` which
   given

   n : nonnegative integer

   returns `true` if `n` can be the hypotenuse of a right triangle
   with integer side lengths (i.e., there are `a` and `b` such that `a
   * a + b * b = n * n` is `true`).

   Examples:
   let _ = assert (py_trip_hyp 5)
   let _ = assert (py_trip_hyp 13)
   let _ = assert (py_trip_hyp 17)
   let _ = assert (py_trip_hyp 29)
   let _ = assert (not (py_trip_hyp 28))
   let _ = assert (not (py_trip_hyp 6))

*)

let sqrt n =
  let rec go i =
    if i * i < n
    then go (i + 1)
    else i
  in go 0

let is_perfect_square n =
  let r = sqrt n in
  r * r = n

let py_trip_hyp n =
  let rec go i =
    if i = 0
    then false
    else
      (n - i * i) > 0
      && is_perfect_square (n - i * i)
      || go (i - 1)
  in go (n - 1)
