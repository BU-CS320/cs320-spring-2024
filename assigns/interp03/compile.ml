#use "interp_03.ml";;

exception TopParseError

let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match compile input with
  | None -> raise TopParseError
  | Some t -> print_string t

let _ = main ()
