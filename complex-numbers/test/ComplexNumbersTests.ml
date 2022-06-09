open ComplexNumbers;;
open OUnit2;;

(* unit tests
     these test add/mult with specific inputs and outputs *)

(* The expected result should be (CI(1,1)) *)
let cadd_test1 _test_ctxt =
  assert_equal (CI(1,1)) (cadd (CI(1,0)) (CI(0,1)));;

(* The expected result should be (CI(10,10)) *)
let cadd_test2 _test_ctxt =
  assert_equal (CI(10,10)) (cadd (CI(6,4)) (CI(4,6)));;

(* The expected result should be (CI(-11,23)) *)
let cmult_test1 _test_ctxt =
  assert_equal (CI(-11,23)) (cmult (CI(3,2)) (CI(1,7)));;

(* The expected result should be (CI(-8,49)) *)
let cmult_test2 _test_ctxt =
  assert_equal (CI(-8,49)) (cmult (CI(5,2)) (CI(2,9)));;

(* The expected result should be (CI(17,17)) *)
let cmult_test3 _test_ctxt =
  assert_equal (CI(17,17)) (cmult (CI(4,1)) (CI(5,3)));;

(* The expected result should be (CI(-50,150)) *)
let cmult_test4 _test_ctxt =
  assert_equal (CI(-50,150)) (cmult (CI(10,10)) (CI(5,10)));;

(* list of unit tests *)
let unit_tests =
    ["cadd_test1">:: cadd_test1;
     "cadd_test2">:: cadd_test2;
     "cmult_test1">:: cmult_test1;
     "cmult_test2">:: cmult_test2;
     "cmult_test3">:: cmult_test3;
     "cmult_test4">:: cmult_test4;
    ];;

(* property tests
 *   these check algebraic laws for complex numbers
 *
 * using some algebraic laws from
     https://proofwiki.org/wiki/Properties_of_Complex_Numbers *)

(* complex number generator *)
let complex_number_gen =
  (QCheck.Gen.map (fun (x,y) -> CI (x,y))
     QCheck.Gen.(pair small_nat small_nat));;

(* complex number pretty printers to show counter examples discovered
 * by quick check *)

let show_complex_numbers1 (CI(a,b)) =
  "Pair (" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n" ;;

let show_complex_numbers2 (CI(a,b),CI(c,d)) =
  "Pair (" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
  ^ "Pair (" ^ string_of_int c ^ "," ^ string_of_int d ^ ")\n" ;;

let show_complex_numbers3 (CI(a,b),CI(c,d),CI(e,f)) =
  "Pair (" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
  ^ "Pair (" ^ string_of_int c ^ "," ^ string_of_int d ^ ")\n"
  ^ "Pair (" ^ string_of_int e ^ "," ^ string_of_int f ^ ")\n" ;;

(* add is commutative:
   z1 + z2 = z2 + z1 *)
let add_commutative =
  QCheck.Test.make ~name:"cadd_commutative" ~count:10000 
    QCheck.(make
              ~print:show_complex_numbers2
              (Gen.pair complex_number_gen complex_number_gen))
    (fun (ci1,ci2) -> cadd ci1 ci2 = cadd ci2 ci1);;

(* mult is commutative:
   z1 * z2 = z2 * z1 *)
let mult_commutative =
  QCheck.Test.make ~name:"cmult_commutative" ~count:10000
    QCheck.(make
              ~print:show_complex_numbers2
              (Gen.pair complex_number_gen complex_number_gen))
    (fun (ci1,ci2) -> cmult ci1 ci2 = cmult ci2 ci1);;

(* add is associative:
 * z1 + (z2 + z3) = (z1 + z2) + z3  *)
let add_associative =
  QCheck.Test.make ~name:"cadd_associative" ~count:10000
    QCheck.(make
              ~print:show_complex_numbers3
              (Gen.triple complex_number_gen complex_number_gen complex_number_gen))
    (fun (ci1,ci2,ci3) ->
      (cadd ci1 (cadd ci2 ci3)) = (cadd (cadd ci1 ci2) ci3));;

(* Associative law for multiplication:
 * z1 * (z2 * z3) = (z1 * z2) * z3 *)
let mult_associative =
  QCheck.Test.make ~name:"mult_associative" ~count:10000
    QCheck.(make
              ~print:show_complex_numbers3
              (Gen.triple complex_number_gen complex_number_gen complex_number_gen))
    (fun (ci1,ci2,ci3) -> cmult ci1 (cmult ci2 ci3) = (cmult (cmult ci1 ci2)) ci3);;

(* Multiplication is distributive with respect to addition:
 * z1 * (z2 + z3) = z1 * z2 + z1 * z3 *)
let mult_distributive =
  QCheck.Test.make ~name:"mult_distributive" ~count:10000
    QCheck.(make
              ~print:show_complex_numbers3
              (Gen.triple complex_number_gen
                 complex_number_gen
                 complex_number_gen))
    (fun (ci1,ci2,ci3) -> cmult ci1 (cadd ci2 ci3) = cadd (cmult ci1 ci2) (cmult ci1 ci3));;  

(* numeric property tests *)

(* cmult of any complex number applied to CI(0,0) should be CI(0,0)
 * Check with operands in either order i.e.
 *   `cmult ci1 (CI(0,0))`  and `cmult ci1 (CI(0,0))`
 *)
let mult_zero =
  QCheck.Test.make ~name:"cmult_zero" ~count:10000
    QCheck.(make
              ~print:show_complex_numbers1
              (complex_number_gen))
    (fun ci1 ->
      cmult (CI(0,0)) ci1 = (CI(0,0))
      && cmult ci1 (CI(0,0)) = (CI(0,0))) ;;

(* cadd of any complex number applied to CI(0,0) should be that
 * complex number. Check with operands in either order i.e.
 *   `cadd ci1 (CI(0,0))`  and `cadd ci1 (CI(0,0))`
 *)
let add_identity =
  QCheck.Test.make ~name:"cadd_identity" ~count:10000
    QCheck.(make
              ~print:show_complex_numbers1
              (complex_number_gen))
    (fun ci1 -> cadd ci1 (CI(0,0)) = ci1) ;;

(* list of all property tests *)                  
let property_tests =
  List.map QCheck_ounit.to_ounit2_test
    [ add_commutative
    ; mult_commutative
    ; add_associative
    ; mult_associative
    ; mult_distributive
    ; mult_zero
    ; add_identity
    ];;

(* run the unit and property based tests *)
let () =
  run_test_tt_main
    ("complex_number_tests">:::
       (List.append unit_tests property_tests));;
