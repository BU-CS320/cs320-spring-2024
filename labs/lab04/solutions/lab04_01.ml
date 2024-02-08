(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

   Note to TA/TFs and myself: work through how would approach this problem
*)

let taxicab (n : int) : int = assert false (* TODO *)

(* PERFECT CUBE *)
let is_perfect_cube (n : int) : bool =
  let rec loop i =
    let cube = i * i * i in
    if cube > n then false else if cube = n then true else loop (i + 1)
  in
  loop 0

let taxicab (n : int) : int =
  let rec loop a =
    let cube_a = a * a * a in
    if cube_a > n / 2 then 0
    else (if is_perfect_cube (n - cube_a) then 1 else 0) + loop (a + 1)
  in
  loop 1

(* PERFECT CUBE - TAIL RECURSIVE *)
let taxicab (n : int) : int =
  let rec loop a acc =
    let cube_a = a * a * a in
    if cube_a > n / 2 then acc
    else loop (a + 1) (acc + if is_perfect_cube (n - cube_a) then 1 else 0)
  in
  loop 1 0

(* SINGLE LOOP BRUTE *)
let taxicab (n : int) : int =
  let rec loop a b =
    let cube_a = a * a * a in
    let cube_b = b * b * b in
    if cube_a > n then 0
    else if cube_a + cube_b > n then loop (a + 1) (a + 1)
    else if cube_a + cube_b = n then 1 + loop (a + 1) (a + 1)
    else loop a (b + 1)
  in
  loop 1 1

(* SINGLE LOOP BRUTE - TAIL RECURSIVE *)
let taxicab (n : int) : int =
  let rec loop a b acc =
    let cube_a = a * a * a in
    let cube_b = b * b * b in
    if cube_a > n then acc
    else if cube_a + cube_b > n then loop (a + 1) (a + 1) acc
    else if cube_a + cube_b = n then loop (a + 1) (a + 1) (acc + 1)
    else loop a (b + 1) acc
  in
  loop 1 1 0

(* DOUBLE LOOP *)
let taxicab (n : int) : int =
  let rec loop a =
    let cube_a = a * a * a in
    if cube_a > n then 0
    else
      let rec loop2 b =
        let cube_b = b * b * b in
        if cube_a + cube_b > n then false
        else if cube_a + cube_b = n then true
        else loop2 (b + 1)
      in
      (if loop2 a then 1 else 0) + loop (a + 1)
  in
  loop 1

(* DOUBLE LOOP - TAIL RECURSIVE *)
let taxicab (n : int) : int =
  let rec loop a acc =
    let cube_a = a * a * a in
    if cube_a > n then acc
    else
      let rec loop2 b =
        let cube_b = b * b * b in
        if cube_a + cube_b > n then false
        else if cube_a + cube_b = n then true
        else loop2 (b + 1)
      in
      loop (a + 1) (acc + if loop2 a then 1 else 0)
  in
  loop 1 0
