let maximum (leq : 'a -> 'a -> bool) (l : 'a list) : 'a =
  assert false (* TODO *)











































(* A SOLUTION *)

let max leq x y = if leq x y then y else x

let max_op leq x y =
  match x with
  | None -> Some y
  | Some v -> Some (max leq v y)

let maximum (leq : 'a -> 'a -> bool) (l : 'a list) : 'a option =
  let rec go l curr_max =
    match l with
    | [] -> curr_max
    | x :: xs -> go xs (max_op leq curr_max x)
  in go l None
  (*
  if List.is_empty l then
    None
  else
    let rec go l curr_max =
      match l with
      | [] -> curr_max
      | x :: xs -> go xs (max leq x curr_max)
    in Some (go l (List.hd l))
  *)

let maximum (leq : 'a -> 'a -> bool) (l : 'a list) : 'a option =
  List.fold_left (max_op leq) None l
  (*
  if List.is_empty l then
    None
  else
    Some (List.fold_left (max leq) (List.hd l) l)
  *)


