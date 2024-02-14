let rec next_word (cs : char list) =
  match cs with
  | [] -> []
  | x :: xs ->
    if (not (x = ' ')) then x :: next_word xs else []

let rec pos_prefix (l : int list) : int list =
  match l with
  | [] -> []
  | x :: xs ->
    if x > 0 then x :: pos_prefix xs else []




































(* A solution
let take_while p =
  let rec go l =
    match l with
    | [] -> []
    | x :: xs -> if p x then x :: go xs else []
  in go
*)