(* String Triangle

   Implement the function `string_tri` of type `string -> string`
   which given

   s : string conisting only of capital english letters [A-Z]

   returns the string with the same characters but organized as a
   right triangle.  If the last line as too few letters, then pad it with '*'.

   Examples:
   let _ = assert (string_tri "ABCDE" = "A\nBC\nDE*")
   let _ = assert (string_tri "ABCDEFGH" = "A\nBC\nDEF\nGH**")
   let _ = assert (string_tri "AAA" = "A\nAA")

*)

let string_tri s =
  let rec go min s =
    if String.length s <= min
    then s ^ String.make (min - String.length s) '*'
    else
      let top = String.sub s 0 min in
      let rest = String.sub s min (String.length s - min) in
      top ^ "\n" ^ go (min + 1) rest
  in
  go 1 s

let _ = assert (string_tri "ABCDE" = "A\nBC\nDE*")
let _ = assert (string_tri "ABCDEFGH" = "A\nBC\nDEF\nGH**")
let _ = assert (string_tri "AAA" = "A\nAA")
