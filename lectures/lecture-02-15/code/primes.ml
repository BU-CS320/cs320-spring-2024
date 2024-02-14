let rec range i j =
  if i > j then
    []
  else
    i :: range (i + 1) j

let primes n =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | x :: xs -> go (x :: acc) (List.filter (fun i -> i mod x <> 0) xs)
  in go [] (range 2 n)



