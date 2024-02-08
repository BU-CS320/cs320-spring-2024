(* CS 320, Feb 8, 2024 *)

open List

(* The following are equivalent ways of defining the function foo:
     let foo p1 p2 ... pn = expr
     let foo = function p1 -> function p2 -> ... -> function pn -> expr
     let foo = fun p1 p2 ... pn -> expr
*)

let head : 'a list -> 'a option = function
  | [] -> None
  | (h :: _) -> Some h
let tail : 'a list -> 'a list option = function
  | [] -> None
  | (_ :: t) -> Some t

type 'a matrix = 'a list list
type int_matrix = int matrix

let mat1 = [[1 ;2 ;3] ; [4 ;5 ;6]]
(* mat1 represents the 2-by-3 matrix
   | 1 2 3 |
   | 4 5 6 |
*)
let mat2 = [[1 ;2] ; [3;4] ; [5 ;6]]
(* mat2 represents the 3-by-2 matrix
   | 1 2 |
   | 3 4 |
   | 5 6 |
*)
let mat3 = [[1 ;2 ;3] ; [4 ;5 ]]
(* mat3 represents the truncated matrix
   | 1 2 3 |
   | 4 5   |
*)

let rec row_lengths (m : 'a matrix) : int list =
    match m with
    | [] -> []
    | (h :: t) -> (length h) :: row_lengths t

let well_formed (m : 'a matrix) : bool =
   let rows = row_lengths m       
   in  let rec help = function
       | []  -> true             (* empty matrix is well-formed *)
       | [_] -> true    
       | h1 :: h2 :: t ->  
             if h1 <> h2 then false else help (h2 :: t)
        in help rows 

let dimension (m : 'a matrix) : (int * int) option =
    match (well_formed m) with
    | false -> None
    | true  -> let no_rows = length m
               in let no_cols =
                    match m with
                    | [] -> 0
                    | r :: t -> length r
               in Some (no_rows,no_cols)

(* For special case of matrices:
   [[]] = matrix with 1 empty row,
   [[];[]] = matrix with 2 empty rows,
   [[];[];[]] = matrix with 3 empty rows, etc.,
   the function dimension returns the following values:
   Some (1,0), Some (2,0), Some (3,0), etc.
*)

let same_dim (m1 : 'a matrix) (m2 : 'a matrix) : bool =
   if   (well_formed m1) = false || (well_formed m2) = false
   then false 
   else (dimension m1) = (dimension m2)  

(********
   addition of integer matrices with same dimension:
   | 1 2 3 | + | 1 1 1 |  =  | 2 3 4 |
   | 4 5 6 |   | 2 2 2 |     | 6 7 8 |
********)

(* function 'add_int_lists' is NOT restricted to lists of same length,
   though we use it in 'add_int_mats' when int lists have same length *)
let rec add_int_lists (l1 : int list) (l2 : int list) : int list =
   match (l1,l2) with
   | ([],_) -> []
   | (_,[]) -> []
   | (h1 :: t1, h2 :: t2) -> (h1 + h2) :: (add_int_lists t1 t2)

let add_int_mats (m1 : int matrix) (m2 : int matrix) : (int matrix) option =
   if (same_dim m1 m2) = false
   then None
   else let rec help mm1 mm2 =
            match (mm1,mm2) with
            | ([],_) -> [] 
            | (_,[]) -> []
            | (h1 :: t1, h2 :: t2) -> (add_int_lists h1 h2) :: (help t1 t2)
        in Some (help m1 m2)

(********
   multiplication of integer matrices of dimensions 2-by-3 and 3-by-2:
   | 1 2 3 | * | 1 2 |  =  | 22 28 |
   | 4 5 6 |   | 3 4 |     | 49 64 |
               | 5 6 |   
********)



