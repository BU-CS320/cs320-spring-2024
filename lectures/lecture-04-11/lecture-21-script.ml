(* Material for CS 320, lecture 21, April 11, 2024 *)

(*
CALL-BY-VALUE (CBV)
The arguments of a function are evaluated and
then their results are passed to the function.
*)

(*
CALL-BY-NAME (CBN)
The arguments of a function are substituted directly 
in the function’s body and evaluated only when necessary.
*)

(*
THUNK
A thunk is a function of type () -> T where T is the type of the function
body. Thunks are used to delay evaluation, i.e., if an argument of a 
function is of thunk type, we can delay its evaluation until needed, 
in a call-by-name style. Thunks simulate CBN in a CBV language.
*)

(* the doubling function *)
let double = fun x -> x + x
let double2 = fun x -> x () + x ()

(*
Try the following expressions after loading this script with:
    # use "lecture_21.ml" ;;

    # double (print_string "hi!!"; 4) ;;  
    # double2  (fun () -> (print_string "hi!!"; 4)) ;;
    
-- if double (print_string “hi!!”; 4) is evaluated using call-by-value, 
the argument will be evaluated once, hi!! will be printed ONCE and the 
return value will be 8.

-- if double (print_string “hi!!”; 4) is evaluated using call-by-name, 
the argument will be copied to the body of the function, which will become: 
(print_string “hi!!”; 4) + (print_string “hi!!”; 4). Then, each part of the 
sum will be evaluated, hi!! will be printed TWICE and the return value will be 8.
*)

let id = fun x -> x    (* polymorphic identity function *) 

let sq : int -> int = fun x -> x * x   (* squaring function on integers *)

let rec myst : int -> int = fun x -> myst x   (* always-diverging function on integers *)

let proj_1 = fun x y -> y ;;      (* the polymorphic first-projection function *)

let proj_2 = fun x y -> x ;;     (* the polymorphic second-projection function *)

(* 
Try the following expressions after loading this script with:
    # use "lecture_21.ml"
If stuck in an infinite loop, suspend utop with "Ctrl z" 

id myst
myst 3
fun (z : int) -> myst 3
(fun (z : int) -> myst 3) 5
proj_1 (myst 3) ;;
proj_1 myst 10               -- int = 10
proj_1 (myst 3) 10           -- diverges
proj_1 (fun z -> myst 3) 10  -- int = 10 
*)

let foo () = print_endline "Argument evaluated!! Argument evaluated!!";;

let cbv_fn f =          (* Note: f is a dummy argument *)
    print_endline "=> Not using the argument, evaluated anyway." ;;
let cbn_fn f =          (* Note: f is a dummy argument *)
    print_endline "=> Not using the argument, not evaluated."  ;;
let bar _ = 
    print_endline "CALL-BY-VALUE:";
        cbv_fn (foo ());
    print_endline "CALL-BY-NAME:";
        cbn_fn (fun () -> foo ())     
        (* (fun () -> foo ()) is a thunk and evaluation of foo ()
        is thus suspended by wrapping it in a lambda *)
      ;;