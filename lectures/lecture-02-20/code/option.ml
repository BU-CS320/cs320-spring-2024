let map f oa =
  let rec go oa =
    match oa with
    | None -> None
    | Some x -> Some (f x)
  in go oa

let map f ra =
  let rec go ra =
    match ra with
    | Error e -> Error e
    | Ok a -> Ok (f a)
  in go ra

let fold f base am =
  let rec go am =
    match am with
    | None -> base
    | Some x -> f base x
  in go am

(* Example based on Option.value *)
let value def ma = fold (fun _ x -> x) def ma



