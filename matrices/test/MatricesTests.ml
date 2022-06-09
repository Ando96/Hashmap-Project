open Matrices;;
open OUnit2;;
(* unit tests *)

let matrix_empty : intseq list = [];;

(* testing if empty matrix is a valid matrix (ismatrix) - should return true*)
let ismatrix_empty_test1 _test_ctxt =
  assert_bool "ismatrix should return true" (ismatrix matrix_empty);;

(* testing the empty list - should return true*)
let matrix_one_row_zero_columns : intseq list = [[]];;

(* testing if matrix with one empty row is a valid matrix (ismatrix) - should return true*)
let ismatrix_empty_test2 _test_ctxt =
  assert_bool "ismatrix should return true" (ismatrix matrix_one_row_zero_columns);;

let matrix_one_row : intseq list = [[1]];;

(* testing ismatrix - should return true, as one row is still of equal length*)
let ismatrix_one_row _test_ctxt =
  assert_bool "ismatrix should return true" (ismatrix matrix_one_row);;

let matrix_two_rows : intseq list  = [[1];[2]];;

(* testing ismatrix with matrx_two_rows - should return true,
   as two rows is still of equal length *)
let ismatrix_two_rows _test_ctxt =
  assert_bool "ismatrix should return true" (ismatrix matrix_two_rows);;

let matrix_three_rows : intseq list  = [[1];[2];[3]] ;;

(* testing ismatrix with matrx_three_rows - should return true,
   as two rows is still of equal length *)
let ismatrix_three_rows _test_ctxt =
  assert_bool "ismatrix should return true" (ismatrix matrix_three_rows);;

let matrix_three_rows_two_columns : intseq list = [[1;2];[2;3];[3;4]];;

(* testing ismatrix with matrx_three_rows *)
let ismatrix_three_rows_two_columns _test_ctxt =
  assert_bool "ismatrix should return true" (ismatrix matrix_three_rows_two_columns);;

let matrix_four_rows_four_columns : intseq list = [[1;2;3;4];[2;3;4;5];[3;4;5;6];[1;2;3;4]];;

(* testing ismatrix with matrx_four_rows *)
let ismatrix_four_rows_four_columns _test_ctxt =
  assert_bool "ismatrix should return true" (ismatrix matrix_four_rows_four_columns);;

let not_a_matrix1 : intseq list = [[1;2];[2;3];[3;4;5;6]];;

(* testing ismatrix with not_a_matrix1 *)
let ismatrix_no1 _test_ctxt =
  assert_bool "ismatrix should return true" (not (ismatrix not_a_matrix1));;

let not_a_matrix2 : intseq list = [[1;2;3];[2;3;4;5];[3;4;5;6]];;

(* testing ismatrix with not_a_matrix2 *)
let ismatrix_no2 _test_ctxt =
  assert_bool "ismatrix should return true" (not (ismatrix not_a_matrix2));;

let matrixshape_empty _test_ctxt =
  assert_equal (0,0) (matrixshape matrix_empty);;

let matrix_two_rows_three_columns : intseq list = [[2;3;4];[1;2;3]];;

(* test for the correct shape of matrix_two_rows_three_columns *)
let matrixshape_two_rows_three_columns _test_ctxt =
  assert_equal (3,2) (matrixshape matrix_two_rows_three_columns);;

let matrix_one_row_one_column : intseq list = [[1]];;

(* test for the correct shape of matrix_one_row_one_column *)
let matrixshape_one_row_one_column _test_ctxt =
  assert_equal (1,1) (matrixshape matrix_one_row_one_column);;

(* test for the correct shape of matrix_one_row_zero_columns *)
let matrixshape_one_row_zero_columns _test_ctxt =
  assert_equal (0,1) (matrixshape matrix_one_row_zero_columns);;

let matrix_four_rows_four_columns : intseq list = [[1;2;3;4];[2;3;4;5];[3;4;5;6];[1;2;3;4]];;

(* test for the correct shape of matrix_four_rows_four_columns *)
let matrixshape_four_rows_four_columns _test_ctxt =
  assert_equal (4,4) (matrixshape matrix_four_rows_four_columns) ;;

(* test adding matrix_one_row_zero_columns to itself *)
let matrixadd_empties _test_ctxt =
  assert_equal ([[]]) (matrixadd matrix_one_row_zero_columns matrix_one_row_zero_columns);;

let matrix_4x4_1 : intseq list  = [[1;2;3;4];[2;3;4;5];[3;4;5;6];[1;2;3;4]];;
let matrix_4x4_2 : intseq list  =[[1;2;3;4];[2;3;4;5];[3;4;5;6];[1;2;3;4]];;

(* test adding matrix_4x4_1 to matrix_4x4_2  *)
let matrixadd_4x4 _test_ctxt =
  assert_equal
    ([[2;4;6;8];[4;6;8;10];[6;8;10;12];[2;4;6;8]])
    (matrixadd matrix_4x4_1 matrix_4x4_2);;

let matrix_4x1_1 : intseq list = [[2];[2];[-1];[-1]] ;;
let matrix_4x1_2 : intseq list = [[-1];[-1];[1];[1]] ;;

(* test adding matrix_4x1_1 to matrix_4x1_2  *)
let matrixadd_4x1 _test_ctxt =
  assert_equal
    ([[1];[1];[0];[0]])
    (matrixadd matrix_4x1_1 matrix_4x1_2);;

let matrix_3x3_1 : intseq list = [[5;6;7];[1;4;6];[6;8;3]] ;;
let matrix_3x3_2 : intseq list = [[4;7;8];[3;6;8];[1;2;3]] ;;

(* test adding matrix_3x3_1 to matrix_3x3_2  *)
let matrixadd_3x3 _test_ctxt =
  assert_equal
    ([[9;13;15];[4;10;14];[7;10;6]])
    (matrixadd matrix_3x3_1 matrix_3x3_2);;

(* multiplying matrix_empty to itself should be an empty matrix  *)
let matrixmult_empty _test_ctxt =
  assert_equal
    ([])
    (matrixmult matrix_empty matrix_empty);;

let matrixmult_zero_columns _test_ctxt =
  let matrix1 : intseq list  = [[]] in
  let matrix2 : intseq list = [[]] in
  assert_equal
    ([[]])
    (matrixmult matrix1 matrix2);;

(* test multiplying two 3x3 matrices together *)
let matrixmult_3x3 _test_ctxt =
  let matrix1 : intseq list = [[5;6;7];[1;4;6];[6;8;3]] in
  let matrix2 : intseq list  = [[4;7;8];[3;6;8];[1;2;3]] in
  assert_equal
    ([[45;85;109];[22;43;58];[51;96;121]])
    (matrixmult matrix1 matrix2);;

(* test multiplying two 3x2 matrices together *)
let matrixmult_3x2 _test_ctxt =
  let matrix1 : intseq list = [[3;4];[4;5]] in
  let matrix2 : intseq list = [[2;1;4];[4;5;2]] in
  assert_equal
    ([[22;23;20];[28;29;26]])
    (matrixmult matrix1 matrix2);;

(* test multiplying two 2x4 matrices together *)
let matrixmult_2x4 _test_ctxt =
  let matrix1 : intseq list = [[3;4];[4;5];[3;1];[2;3]] in
  let matrix2 : intseq list= [[2;1;4];[2;3;4]] in
  assert_equal
    ([[14;15;28];[18;19;36];[8;6;16];[10;11;20]])
    (matrixmult matrix1 matrix2);;

(* list of unit tests *)
let unit_tests =
  [ (* ismatrix tests *)
    "ismatrix_empty1">::ismatrix_empty_test1
  ; "ismatrix_empty2">::ismatrix_empty_test2
  ; "ismatrix_one_row">::ismatrix_one_row
  ; "ismatrix_two_rows">::ismatrix_two_rows
  ; "ismatrix_three_rows">::ismatrix_three_rows
  ; "ismatrix_three_rows_two_columns">::ismatrix_three_rows_two_columns
  ; "ismatrix_four_rows_four_columns">::ismatrix_four_rows_four_columns
  ; "ismatrix_no1">::ismatrix_no1
  ; "ismatrix_no2">::ismatrix_no2

  (* matrix shape tests *)
  ; "matrixshape_empty">::matrixshape_empty
  ; "matrixshape_two_rows_three_columns">::matrixshape_two_rows_three_columns
  ; "matrixshape_one_row_one_column">::matrixshape_one_row_one_column
  ; "matrixshape_one_row_zero_columns">::matrixshape_one_row_zero_columns
  ; "matrixshape_four_rows_four_columns">::matrixshape_four_rows_four_columns

  (* matrix addition tests *)
  ; "matrixadd_empties">::matrixadd_empties
  ; "matrixadd_4x4">::matrixadd_4x4
  ; "matrixadd_4x1">::matrixadd_4x1
  ; "matrixadd_3x3">::matrixadd_3x3
(*
  (* matrix multiplication tests *)
  ; "matrixmult_empty">::matrixmult_empty
  ; "matrixmult_zero_columns">::matrixmult_zero_columns
  ; "matrixmult_3x3">::matrixmult_3x3
  ; "matrixmult_3x2">::matrixmult_3x2
  ; "matrixmult_2x4">::matrixmult_2x4
  *)
  ];;

(* property tests from
 https://en.wikibooks.org/wiki/Famous_Theorems_of_Mathematics/Algebra/Matrix_Theory
 *)

(* pretty printer for counter examples found by quick check *)
let string_of_matrices2 (m1,m2) =
  string_of_matrix m1
  ^ "\n"
  ^ string_of_matrix m2;;

let string_of_matrices3 (m1,m2,m3) =
  string_of_matrix m1
  ^ "\n"
  ^ string_of_matrix m2
  ^ "\n"
  ^ string_of_matrix m3;;

let matrix_gen =
  let open QCheck.Gen in
  (list_size
     (pure 8)
     (list_size (pure 10) nat))
  >>= (fun x -> pure (x)) ;;

let matrix_zeros_gen =
  let open QCheck.Gen in
  (list_size
     (pure 8)
     (list_size (pure 10) (pure 0)))
  >>= (fun x -> pure (x)) ;;

  (* integer sequence generator of 100 elements *)
let integer_sequence_gen =
  QCheck.Gen.(list_size (return 100) nat);;

(* A + B = B + A
  (Commutative law of addition) *)
let commutative_addition =
  QCheck.Test.make ~name:"commutative_addition" ~count:10000
    QCheck.(make
              ~print:string_of_matrices2
              (Gen.pair matrix_gen matrix_gen))
    (fun (m1,m2) ->
       matrixadd m1 m2 = matrixadd m2 m1);;

(* A + ( B + C ) = ( A + B ) + C
   (Associative law of addition) *)
let associative_addition =
  QCheck.Test.make ~name:"associative_addition" ~count:10000
    QCheck.(make
              ~print:string_of_matrices3
              (Gen.triple matrix_gen matrix_gen matrix_gen))
    (fun (m1,m2,m3) ->
       matrixadd m1 (matrixadd m2 m3) = matrixadd (matrixadd m1 m2) m3);;

(* A( BC ) = ( AB )C
   (Associative law of multiplication) *)
let associative_multiplication =
  QCheck.Test.make ~name:"associative_multiplication" ~count:10000
    QCheck.(make
              ~print:string_of_matrices3
              (Gen.triple matrix_gen matrix_gen matrix_gen))
    (fun (m1,m2,m3) -> matrixmult m1 (matrixmult m2 m3) = matrixmult (matrixmult m1 m2) m3);;

(* A( B + C ) = AB + AC
   (Distributive law of matrix algebra)
   Multiplication is distributive with respect to addition *)
let distributive_multiplication =
  QCheck.Test.make ~name:"distributive_multiplication" ~count:10000
    QCheck.(make
              ~print:string_of_matrices3
              (Gen.triple matrix_gen matrix_gen matrix_gen))
    (fun (m1,m2,m3) ->  matrixmult m1 (matrixmult m2 m3) = (matrixmult (matrixmult m1 m2) m3));;

(* numeric property tests *)

(* adding a matrix to a matrix of zero values should return the same
 * matrix *)
let addition_identity =
  QCheck.Test.make ~name:"addition_identity" ~count:10000
    QCheck.(make
              ~print:string_of_matrices2
              (Gen.pair
                 matrix_gen (* m1 *)
                 matrix_zeros_gen (* m2 *)
  ))
    (fun (m1,m2) ->
       matrixadd m1 m2 = m1) ;;

(* multiplying a matrix to a matrix of zero values should return the
   matrix containing zero values *)
let multiplcation_zeros =
  QCheck.Test.make ~name:"multiplication_zeros" ~count:10000
    QCheck.(make
              ~print:string_of_matrices2
              (Gen.pair
                 matrix_gen (* m1 *)
                 matrix_zeros_gen (* m2 *)
  ))
    (fun (m1,m2) -> matrixmult m1 m2 = [[]] );;


(* list of all property tests *)                  
let property_tests =
  List.map QCheck_ounit.to_ounit2_test
    [
      
      commutative_addition
    ; associative_addition
    (*
    ; associative_multiplication
    ; distributive_multiplication
    *)
    ; addition_identity
    (*
    ; multiplcation_zeros
    *)
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("matrices_tests">:::
       (List.append unit_tests property_tests));;