open Essay ;;
open OUnit2;;

(* unit tests *)
let add_test1 _test_ctxt =
  assert_equal 9 (add 5 4);;

let add_test2 _test_ctxt =
  assert_equal 3 (add 1 2);;

let isEven_test1 _ctxt =
  assert_equal
    false
    (isEven (7));;

let isEven_test2 _ctxt =
  assert_equal
    true
    (isEven (4));;   

let reverse_list_test1 _ctxt =
  assert_equal
    []
    (reverse_list []) ;;

let reverse_list_test2 _ctxt =
  assert_equal
    [10;9;8;7;6;5;4;3;2;1]
    (reverse_list [1;2;3;4;5;6;7;8;9;10]) ;; 

let reverse_list_test3 _ctxt =
  assert_equal
    ["John"; "Paul"; "George"; "Ringo"]
    (reverse_list ["Ringo";"George";"Paul";"John"]) ;; 

let any_evens_test1 _ctxt =
  assert_equal
    false
    (any_evens []);;

let any_evens_test2 _ctxt =
  assert_equal
    true
    (any_evens [4;5;6]);;

let length_test1 _ctxt =
  assert_equal
    1
    (length [1]);;

let length_test2 _ctxt =
  assert_equal
    5
    (length [1;2;3;4;5]);;

let double_test1 _ctxt = 
  assert_equal
    4
    (double 2);;

let double_test2 _ctxt = 
  assert_equal
    12
    (double 6);;

let replicate_elem_test1 _ctxt =
  assert_equal
    []
    (replicate_elem  0 ["test"]) ;;

let replicate_elem_test2 _ctxt =
  assert_equal
    ["test";"test";"test"]
    (replicate_elem  3 ["test"]) ;;


(* list of unit tests *)
let unit_tests =
  [ 
    "add_test1">::add_test1;
    "add_test2">::add_test2;
    "isEven_test1">::isEven_test1;
    "isEven_test2">::isEven_test2;
    "reverse_list_test1">::reverse_list_test1;
    "reverse_list_test2">::reverse_list_test2;
    "reverse_list_test3">::reverse_list_test3;
    "any_evens_test1">::any_evens_test1;
    "any_evens_test1">::any_evens_test2;
    "length_test1">::length_test1;
    "length_test2">::length_test2;
    "double_test1">::double_test1;
    "double_test2">::double_test2;
    "replicate_elem_test1">::replicate_elem_test1;
    "replicate_elem_test2">::replicate_elem_test2;
  ];;

(* property based tests *)

let add_zero =
  QCheck.Test.make ~name:"seqmult_zeros" ~count:1000
    QCheck.(make Gen.nat)
    (fun x ->
      add x 0 = x
      && add 0 x = x);;
  
(* list of all property tests *)                  
let property_tests =
  List.map QCheck_ounit.to_ounit2_test
    [ 
      add_zero
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("sequence_arithmetic_tests">:::
       (List.append unit_tests property_tests));;
