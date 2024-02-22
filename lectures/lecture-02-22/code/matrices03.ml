(* CS 320, Feb 22, 2024 *)
(* More simple operations on matrices using higher-order functions:
   map, filter, fold_right, fold_left *)

open List        (* open module List to simplify a few names *)

let head : 'a list -> 'a option = 
  fun xs -> match xs with 
  | [] -> None
  | h :: t -> Some h
let tail : 'a list -> 'a list option = 
  fun xs -> match xs with 
  | [] -> None
  | h :: t -> Some t
 
type 'a matrix = ('a list) list

let mm1 = [ [1;2;3] ]               (* valid 1-by-3 row *)
let mm2 = [ [1;2;3];[4;5;6]]        (* valid 2-by-3 matrix *)  
let mm3 = [ [1]; [2]; [3] ]         (* valid 3-by-1 column *)
let mm4 = [ [1] ; [1;2] ; [3]]      (* invalid matrix, 3 rows of unequal length *) 
let mm5 = []                        (* invalid with-no-row matrix *)
let mm6 = [[]]                      (* invalid with-one-empty-row matrix *)
let mm7 = [[1;2;3];[4;5;6];[7;8;9]] (* valid 3-by-3 square matrix *)

(* testing validity of row *)
let valid_row : 'a matrix -> bool =
  fun m -> match m with
  | [] -> false
  | [rw] -> (length rw > 0)
  | _ -> false 

(* testing validity of column with map and fold_right:
   every row has exactly one entry *)
let valid_col : 'a matrix -> bool =
  fun m -> match m with
  | [] -> false
  | _  -> fold_right (&&) (map (fun xs -> length xs = 1) m) true 

(* testing validity of matrix with map and fold_right:
   every row has the same number of entries as the first row, which is non-zero *)
let valid_mat : 'a matrix -> bool =
  fun m -> match m with
  | [] -> false
  | xs :: _ -> 
    (length xs <> 0) && 
    (fold_right (&&) (map (fun ys -> length xs = length ys) m) true) 

(* testing whether a valid matrix is also a square matrix *)
let valid_sq_mat : 'a matrix -> bool =
  fun m -> match m with
  | [] -> false
  | xs :: _ -> (valid_mat m) && (length xs = length m) 

(* extracting first row with matching *)  
let first_row : 'a matrix -> 'a matrix option =
  fun m -> match (valid_mat m) with 
  | false -> None
  | true  -> match m with
             | [] -> None
             | xs :: _ -> Some [xs]

(* extracting first column, using map *)
let first_col : 'a matrix -> 'a matrix option =
  fun m -> match (valid_mat m) with 
  | false -> None   
  | true  -> Some (map (fun xs -> match xs with
                                  | [] -> []
                                  | ys :: _ -> [ys]) m)

(* computing the dimension of a valid matrix, using fold_left *)                                  
let dimension (m : 'a matrix) : (int * int) option =
    match (valid_mat m) with
    | false -> None
    | true  -> let no_rows = length m
               in let no_cols = 
                    match m with
                    | [] -> 0
                    | xs :: _ -> fold_left (fun a _ -> a + 1) 0 xs
                  in Some (no_rows,no_cols)  

(* diag returns list of entries along main diagonal,
     i.e. 1-st entry of 1-st row, 2-nd entry of 2-nd row, etc. 
     The input matrix m does not have to be valid or square *)                 
let rec diag (m : 'a matrix) : 'a list =
  match m with 
  | row :: rows -> 
      ( match row with 
        | x :: xs -> 
          x :: diag (map (function 
                          | (rw :: rws) -> rws 
                          | [] -> []) 
                     rows)
        | _ -> [] )
  | _ -> []                  
 
