let smallest_prime_factor (n : int) : int =
  assert false (* TODO *)

let p (n : int) : bool =
  assert false (* TODO *)




































(* A SOLUTION *)

let smallest_prime_factor (n : int) : int =
  let rec go i =
    if n mod i = 0 then
      i
    else
      go (i + 1)
  in if n > 1 then go 2 else 1

let p (n : int) =
  if n > 1 then
    let f1 = smallest_prime_factor n in
    let f2 = n / f1 in
    f1 <> f2 && smallest_prime_factor f2 = f2
  else
    false
