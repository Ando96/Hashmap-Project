(* declaration of types intseq and intmatrix *)

type intseq = int list;;

type intmatrix = IM of intseq list;;

(* useful for debugging *)
let string_of_row row =
  String.concat ""
    (List.map (fun x -> string_of_int x ^ " ") row);;

(* useful for debugging *)
let rec string_of_matrix m =
  match m with
          [] -> ""
      | [[]] -> ""
      | (row::rest) ->
         string_of_row row ^ "\n" 
         ^ string_of_matrix rest;;

(* function getbody to retrieve the body of the intmatrix which is of type intseq list *)
let getbody (IM x) = x;;

let rec length : 'a list -> int =
fun xs ->
match xs with
[] -> 0
| (x::rest) -> 1 + length rest;;

let rec seqadd : intseq -> intseq -> intseq =
  fun xs ys ->
  match xs,ys with 
    [] , _  -> []
  | _ , [] -> []
  | (x::xss) , (y::yss) ->
  x + y :: seqadd xss yss;;

  let rec seqmult : intseq -> intseq -> intseq =
  fun xs ys ->
   match xs,ys with 
  [] , _  -> []
  | _ , [] -> []
  | (x::xss) , (y::yss) ->
  x * y :: seqmult xss yss;;

(* test whether a list of lists of integers represents a matrix. 
   The length of each row should be equal.*)

  let rec ismatrix : intseq list -> bool = 
  fun xs ->
  match xs with 
  [] -> true
  | [[]] -> true
  | [row1] -> true
  | (row1 :: row2 :: rest) ->
  if (length row1 = length row2) 
  then ismatrix (row2:: rest)
  else false;;
  
(* function matrixshape takes the matrix, and calculates the number of
   columns and rows *)
let matrixshape : intseq list -> (int * int) =
fun xs ->
 match xs with 
[] -> (0,0)
|[[]] -> (0,1)
 | (x :: rest) -> 
  ((length x), (length xs));;

(* matrix addition *)
let rec matrixadd : intseq list -> intseq list -> intseq list =
fun x y -> match x,y with
[],_ -> x
|_, [] -> y
|(data1row1 :: datarest1), (data2row1 :: datarest2) -> 
 (seqadd data1row1 data2row1) :: matrixadd datarest1 datarest2 ;;

(* matrix multiplication *)
let rec matrixmult : intseq list -> intseq list -> intseq list =
fun x y -> match x,y with
[],_ -> []
|_,[] -> []
|(data1row1 :: datarest1), (data2row1 :: datarest2) ->
 (seqmult data1row1 data2row1) :: matrixmult datarest1 datarest2 ;;

(* (1, 2, 3) • (7, 9, 11) = 1×7 + 2×9 + 3×11
    = 58 *)

 (* (1, 2, 3) • (8, 10, 12) = 1×8 + 2×10 + 3×12
    = 64 *)

(* (4, 5, 6) • (7, 9, 11) = 4×7 + 5×9 + 6×11
    = 139 *)

(* (4, 5, 6) • (8, 10, 12) = 4×8 + 5×10 + 6×12
    = 154 *)