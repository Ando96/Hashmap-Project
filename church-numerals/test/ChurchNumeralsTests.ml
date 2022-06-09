open ChurchNumerals ;;
open OUnit2 ;;

(* test the output from: (c2i (i2c 0)) *)
let c2i_0 _cxt =
  assert_equal (c2i (0)) 0 ;;

(* test the output from: (c2i (i2c 5)) *)
let c2i_1 _cxt =
 assert_equal (c2i (5)) 5 ;;

(* list of unit tests *)
let unit_tests =
    ["c2i_0">:: c2i_0;
     "c2i_1">:: c2i_1
    ];;

(* c2i composed with i2c should be the identity function, i.e.
   
   forall x. c2i (i2c x) = x
*)
let c2i_i2c_identity =
  QCheck.Test.make ~name:"mult_distributive" ~count:10000
    QCheck.(make Gen.nat)
    (fun x -> c2i (x) = x );;

(* list of all property tests *)                  
let property_tests =
  List.map QCheck_ounit.to_ounit2_test
    [
      c2i_i2c_identity
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("church_numerals_tests">:::
       (List.append unit_tests property_tests));;