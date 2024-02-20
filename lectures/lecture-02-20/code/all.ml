let rec all bs =
  match bs with
  | [] -> true
  | false :: _ -> false
  | true :: t -> all t

let all_fold = List.fold_left (&&) true

let l = List.init 100000000 (fun _ -> Random.bool ())
let t = Sys.time ()
let _ = all_fold l
let _ = Printf.printf "all_fold execution time: %fs\n" (Sys.time() -. t)
let t = Sys.time ()
let _ = all l
let _ = Printf.printf "all execution time: %fs\n" (Sys.time() -. t)