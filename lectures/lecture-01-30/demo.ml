type day = M | T | W | Th | F | S | Su

let after_day (d : day) (n: int) : day =
  let int_to_day i =
    match i mod 7 with
    | 0 -> M
    | 1 -> T
    | 2 -> W
    | 3 -> Th
    | 4 -> F
    | 5 -> S
    | 6 -> Su
    | _ -> assert false (* impossible *)
  in
  let day_to_int d =
    match d with
    | M -> 0
    | T -> 1
    | W -> 2
    | Th -> 3
    | F -> 4
    | S -> 5
    | Su -> 6
  in int_to_day (day_to_int d + n)

let _ = assert (after_day W 16 = F)
let _ = assert (after_day W (-1) = T)

let dist (x1, y1) (x2, y2) =
  let xd = x1 -. x2 in
  let yd = y1 -. y2 in
  sqrt (xd *. xd +. yd *. yd)

type point = { x : float ; y : float }
type p_coord = { d : float ; angle : float }

let to_polar (p : point) : p_coord = {
  d = sqrt (p.x *. p.x +. p.y *. p.y) ;
  angle = atan (p.y /. p.x) ;
}