let f x y =
  let y = x + x in
  let x = x + y in
  x

let g () =
  let _ = print_endline "calling g..." in
  0

let _ = f (g ()) (g ())

let f x y =
  let y () = x () + x () in
  let x () = x () + y () in
  x ()

let _ = f g g