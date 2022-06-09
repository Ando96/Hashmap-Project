open Sorting ;;
open OUnit2 ;;

(* unit tests *)

(* unit test sorting an empty list with bubble sort *)
let bubble_sort_empty _test_ctxt =
  assert_equal [] (bubble_sort []);;

(* unit test sorting a list which is in descending order with bubble sort *)
let bubble_sort_reversed _test_ctxt =
  assert_equal [1;2;3] (bubble_sort [3;2;1]);;

(* unit test sorting a list which is in random order with bubble sort *)
let bubble_sort_unordered _test_ctxt =
  assert_equal [1;2;3] (bubble_sort [3;1;2]) ;;

(* unit test sorting a list with duplicate values with bubble sort *)
let bubble_sort_duplicates _test_ctxt =
  assert_equal [3;3;4;4] (bubble_sort [3;4;3;4]);;

(* unit test sorting an empty list with quick sort *)
let quick_sort_empty _test_ctxt =
  assert_equal [] (quick_sort []);;

(* unit test sorting a list which is in descending order with quick sort *)
let quick_sort_reversed _test_ctxt =
  assert_equal [1;2;3] (quick_sort [3;2;1]);;

(* unit test sorting a list which is in random order with quick sort *)
let quick_sort_unordered _test_ctxt =
  assert_equal [1;2;3] (quick_sort [3;1;2]);;

(* unit test sorting a list with duplicate values with quick sort *)
let quick_sort_duplicates _test_ctxt =
  assert_equal [3;3;4;4] (quick_sort [3;4;3;4]);;

(* list of unit tests *)
let unit_tests =
  [
     "bubble_sort_empty">::bubble_sort_empty
   ; "bubble_sort_reversed">::bubble_sort_reversed
   ; "bubble_sort_unordered">::bubble_sort_unordered
   ; "bubble_sort_duplicates">::bubble_sort_duplicates
   ; "quick_sort_empty">::quick_sort_empty
   ; "quick_sort_reversed">::quick_sort_reversed
   ; "quick_sort_unordered">::quick_sort_unordered
   ; "quick_sort_duplicates">::quick_sort_duplicates
  ];;

(* property tests *)

(* check that running bubble sort and quick sort on the same list
 * produces the same result, by comparing them with "=" *)
let bubble_quick_equiv =
  QCheck.Test.make ~name:"bubble_quick_equiv" ~count:100
    QCheck.(make (Gen.list Gen.small_nat))
    (fun xs -> (bubble_sort xs) = (quick_sort xs));;

(* list of all property tests *)                  

(* TODO: uncomment if you want to run this test *)
let property_tests =
  List.map QCheck_ounit.to_ounit2_test
    [
     (* bubble_quick_equiv *) 
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("sorting_tests">:::
       (* (unit_tests));; *)
       (List.append unit_tests property_tests));;