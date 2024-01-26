(* an example with if-expressions *)

let parity_str x =
  if   x < 0
  then "negative"
  else
  if   x > 0
  then "positive"
  else "zero"

let _ = assert (parity_str 2 = "positive")
let _ = assert (parity_str (-2) = "negative")
let _ = assert (parity_str 0 = "zero")

(* an example with let-expression *)

let is_prime n =
  let rec go i =
    if i <= 1
    then false
    else n mod i = 0 || go (i - 1)
  in
  if n <= 1
  then false
  else not (go (n - 1))

let _ = assert (not (is_prime 1))
let _ = assert (is_prime 2)
let _ = assert (is_prime 3)
let _ = assert (not (is_prime 4))
let _ = assert (is_prime 5)
let _ = assert (not (is_prime 6))

(* an example with multiple let-expressions *)

let squared_dist x1 y1 x2 y2 =
  let x_diff = x1 - x2 in
  let y_diff = y1 - y2 in
  x_diff * x_diff + y_diff * y_diff

let _ = assert (squared_dist 3 3 0 1 = 13)
