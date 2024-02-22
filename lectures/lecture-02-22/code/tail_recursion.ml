(* CS 320, February 22, 2024 *)
(* Further discussion on tail recursion,
   also including fold_right and fold_left *) 

(**********************************)
(* EXAMPLE 1: The sum function    *)
(**********************************)

let rec mySum (lst : int list) =
   match lst with
   | [] -> 0
   | x :: xs -> x + mySum xs

(* We transform "mySum" into tail-recursive form by using a technique known
   as "accumulating-parameter style" (APS): *)

let mySumA (lst : int list) =
   let rec mySumAPS ll n =
          match ll with
          | [] -> n
          | x :: xs -> mySumAPS xs (x + n)
   in mySumAPS lst 0

(* Another method for transforming "mySum" into tail-recursive form is
   known "continuation-passing style" (CPS), of which there are many
   variations. Here is one way of writing "mySum" in CPS: *)

let mySumB (lst : int list) =
   let rec mySumCPS ll k =
          match ll with
          | [] -> k 0
          | x :: xs -> mySumCPS xs (fun n -> k (x + n))
   in mySumCPS lst (fun x -> x)

(* For comparison with mySum, mySumA, and mySumB, we now consider two
   implementations using fold_right and fold_left: *)

let mySumC (lst : int list) =
   let foldr = List.fold_right
   in  foldr (+) lst 0

let mySumD (lst : int list) =
   let foldl = List.fold_left
   in  foldl (+) 0 lst

(* In general, we prefer to use fold_right rather than fold_left. However,
   if you look up their definitions, you will find out that fold_right is
   NOT tail recursive, whereas fold_left IS. *)

(****************************)
(* BEGIN DISCUSSION         *)
(****************************)

(*
On a first reading you can skip this discussion -- it is demanding!!

Let's compare these three approaches carefully.  The first program, given
an input list of n integers, will call itself recursively n times.  On the
way down, it does not do any additions, but only remembers the left
argument of the + , and also remembers that an addition must be performed
after the sum of the tail has been calculated. This is called a deferred
operation. It then calls itself recursively on the tail of the input list.
The program continues to call itself recursively on successive tails,
remembering all the deferred operations and the left-hand-sides of + on the
runtime stack, until it gets to the empty list, at which point it returns
0.  Thereafter, on the way back up out of the recursion, at each level it
performs the deferred addition and returns the result to its caller.  Thus
on input [x_1;x_2;...;x_p], the elements are added in the order

   (x_1 + (x_2 + (... + (x_{p-1} + (x_p + 0))...))),

from right to left.

In the second version, i.e. "mySumA", we define an auxiliary function
"mySumAPS" that takes an extra argument n, an accumulated result. The 
initial value of the accumulated result that is passed to the auxiliary 
function "mySumAPS" on the first call is the basis element 0. Thus the 
elements are added in the opposite order:

   (((...((0 + x_1) + x_2) + ...) + x_{p-1}) + x_p),

from left to right.  Luckily in this case, addition is associative, so the
order of additions doesn't matter, and we will get the same result. We
would not be so fortunate if we tried to do the same thing with a
nonassociative operation such as exponentiation or subtraction.

The third version, i.e. "mySumB", is a mixture of both: carry out
the operations from right to left, but with no deferred operations. This is
done using "continuations". Since we have first-class functions, we can
create a function that packages up any such deferred operations, then
passes it down in the recursive call for somebody else to perform. In this
example, the auxiliary function "mySumCPS" takes a continuation function as
an extra argument, which consists of all the deferred operations 
accumulated so far. Before the next recursive call, the deferred addition
is composed with the previously accumulated ones in the correct order, and
this new continuation function is passed down in the next recursive call.
At the bottom of the recursion, all the deferred operations that were
accumulated on the way down are performed all at once by applying the
continuation to the basis element 0.

The initial call to "mySumCPS" passes the identity function (fun x -> x),
which is the identity element for the operation of function composition.
On the way down, the operation of adding the new element x is composed with
the passed-in continuation k consisting of all deferred additions
accumulated so far, giving a new continuation (fun n -> k (x + n)).

If we use the period "." to denote function composition, then
(fun n -> k (x + n)) is equivalent to "k . (fun n -> x + n)", i.e.,
the composition of k and (fun n -> x + n), with (fun n -> x + n) applied
first and k applied second.

Thus at the bottom of the recursion on input [x_1;x_2;...;x_p], we have a
function that is equivalent to the composition of p+1 functions:

(fun x -> x).(fun n -> x_1 + n).(fun n -> x_2 + n). ... .(fun n -> x_p + n)

Function composition is a binary operation that is associative, i.e.,
(f . g) . h = f . (g . h) and the parentheses are superfluous. We omit 
all superfluous parentheses for clarity. Applying the preceding function 
to 0 gives: 

   (x_1 + (x_2 + (... + (x_{p-1} + (x_p + 0))...))),

which is the same sum calculated in the same order as in the first version.

*)

(***********************)
(* END DISCUSSION      *)
(***********************)

(**********************************)
(* EXAMPLE 2: The 3*x + 1 Problem *)
(**********************************)

(* The 3*x+1 problem refers to the following iteration.  Given a positive
   integer x:

     if x is even, divide it by 2 
     if x is odd, multiply it by 3 and add 1. 
     repeat forever. 

  This process eventually ends up in the cycle 1 -> 4 -> 2 -> 1 for all x 
  that anyone has every tried, but no one has yet been able to prove that
  this occurs for all x. *)

(* Here are three programs that perform this iteration, returning the
   sequence of integers produced along the way until 1 is encountered.
   These three programs correspond to the three versions of "sum"
   above: ordinary non-tail recursion, tail recursion using APS, and
   tail recursion using CPS. *)

(* Ordinary non-tail recursion: *)

let is_even d =
   (d mod 2) = 0
 
let rec foo (x : int) : int list =
   match x with
   | 1 -> [1]
   | _ -> if is_even x then x :: foo (x / 2)
                       else x :: foo (3 * x + 1)

(* Tail-recursion, using accumulating-parameter style: *)

let rec fooA ((x,a) : int * int list) : int list  =  
   match x with
   | 1 -> a @ [1]
   | _ -> if is_even x then fooA (x / 2, a @ [x])
                       else fooA (3 * x + 1, a @ [x])

(* "fooA" is activated by evaluating "fooA (x,[])" *)

(* Tail-recursion, using continuation-passing style: *)

let rec fooB ((x,k) : int * (int list -> int list)) : int list =
   match x with
   | 1 -> k [1]
   | _ -> if is_even x then fooB (x / 2, fun a -> k (x :: a))
                       else fooB (3 * x + 1, fun a -> k (x :: a))

(* "fooB" is activated by evaluating "fooB (x,fun x -> x)" *)

(***********************) 
(* BEGIN DISCUSSION    *)
(***********************)

(* Note that foo and fooB use (::), a constant-time operation, whereas
   the tail-recursive version fooA uses the linear-time (@).  This is
   because of the associativity issues discussed above.  We could fix
   this by producing the list in the opposite order and then reversing
   it at the end, but this is not the point.  *)

(***********************) 
(* END DISCUSSION      *)
(***********************)


(***********************************)
(* EXAMPLE 3: The "split" function *)
(***********************************)

(* The "split" function splits an input list into two equal
   sublists. A good definition of "split" avoids having to compute the
   length of the input list, and the following definition accomplishes
   this. It uses two help functions, "evens" and "odds", but note that
   neither is tail-recursive. *)

(* In this example, simplicity and transparency of the code are lost when
   we transform it into tail-recursive form. *)

let rec evens lst =
   match lst with
   |           [] -> []
   |          [x] -> [x]
   | x :: _ :: xs -> x :: evens xs

let rec odds lst =
   match lst with
   |           [] -> []
   |          [x] -> []
   | _ :: x :: xs -> x :: odds xs

let split lst = (evens lst, odds lst) 

(* We can re-write the preceding definition in order to make "evens"
   and "odds" local rather than global: *)

let splitA lst =
   let rec evens ll =
      match ll with
      | [] -> []
      | [x] -> [x]
      | x :: _ :: xs -> x :: evens xs
   and odds ll =
      match ll with
      | [] -> []
      | [x] -> []
      | _ :: x :: xs -> x :: odds xs 
   in (evens lst, odds lst)

(* Following the APS and CPS transformations for functions "mySum" (in
   Example 1) and "foo" (in Example 2), we now transform "split" to
   put it in tail-recursive form. *)

(* First, in accumulating-parameter style, we obtain: *)

let splitAPS lst =
   let rec splitAPS' ll (l1,l2) =   
       match ll with
       | [] -> (l1,l2)
       | [x] -> (x :: l1, l2)   
       | x1 :: x2 :: xs -> splitAPS' xs (x1 :: l1, x2 :: l2)
   in splitAPS' lst ([],[])

(* Second, in continuation-passing style, we obtain: *)

let splitCPS lst =
   let rec splitCPS' ll k =
       match ll with
       | [] -> k ([], [])
       | [x] -> k ([x], [])
       | x1 :: x2 :: xs -> splitCPS' xs (fun (l1,l2) -> k (x1::l1,x2::l2))
   in splitCPS' lst (fun y -> y)

(* EXERCISE *)

(* For the same input argument, there is a difference in the outputs
   of the function splitAPS and splitCPS. What is this difference?
   Adjust the code of splitAPS so that its output is identical to that
   of splitCPS. *)

