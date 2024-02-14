type 'a vec = {
  length : int ;
  vals : 'a list ;
}

(* One approach
let rec sum l =
  match l with
  | [] -> 0.
  | x :: xs -> x +. sum xs

let rec squares l =
  match l with
  | [] -> []
  | x :: xs -> x *. x :: squares xs

let rec normalize v =
  let norm = sqrt (sum (squares v.vals)) in
  let rec go l =
    match l with
    | []-> []
    | x :: xs -> x /. norm :: go xs
  in { v with vals = go v.vals }
*)

let rec normalize v =
  let sum l = List.fold_right (+.) l 0. in
  let square x = x *. x in
  let norm =
    v.vals
    |> List.map square
    |> sum
    |> sqrt
  in
  let div_norm x = x /. norm in
  let vals =
    v.vals
    |> List.map div_norm
  in { v with vals }

