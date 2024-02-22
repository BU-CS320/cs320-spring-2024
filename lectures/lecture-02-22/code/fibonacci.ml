(* CS 320, 20 February 2024 *)
(* small example illustrating memoization -- not in full generality *)

open List

let head : 'a list -> 'a option = 
  fun xs -> match xs with
  | [] -> None
  | (h :: _) -> Some h
let tail : 'a list -> 'a list option = 
  fun xs -> match xs with
  | [] -> None
  | (_ :: t) -> Some t
let reverse l =
  let rec help acc = 
    fun xs -> match xs with 
    | [] -> acc
    | hd :: tl -> help (hd :: acc) tl 
  in help [] l 
let first_two : 'a list -> 'a list = 
  fun xs -> match xs with 
  | [] -> []
  | [x] -> [x]
  | x :: y :: _ -> [x;y]

(* standard definition of fibonacci *)
let rec fib1 : int -> int =
  fun x -> if x <= 1 then 1 else fib1 (x-1) + fib1 (x-2) ;;

(* fib_pair is help function for memoized fibonacci *)  
let rec fib_pair : int -> (int * int) =
  fun x -> 
  if x < 1  then (0,1) else
  if x = 1  then (1,1) else 
        let (y,z) = fib_pair (x-1)
        in (z,y+z) 

(* definition of a memoized fibonacci *)        
let fib2 : int -> int = fun x -> snd (fib_pair x)

(* fibonacci numbers in ascending order using fib1 -- can't go beyond 30 or 40 *)
let rec fib_seq1 : int -> int list =
  fun x -> if x < 0 then [] else fib_seq1 (x-1) @ [fib1 x]
 
(* fibonacci numbers in ascending order using fib2 *)  
let rec fib_seq2 : int -> int list =
  fun x -> if x < 0 then [] else fib_seq2 (x-1) @ [fib2 x]
