open OUnit2;;

(*******************************) 

(* Start of answer to Question M 

Write a function 

   sumf : 'a list -> ('a -> int) -> int 

that inputs a list l and a function f : int -> int and outputs 

   the sum of f applied to all the elements of l 

(so sumf [1;2;3] (fn x -> x*x) calculates 1*1+2*2+3*3 = 14). 

*) 

let rec sumf xs f =
  match xs with
    (* If the list is empty then the sum of the empty list is 0 *) 
    [] -> 0 
  (* Otherwise calculate (f h) and proceed recursively *) 
  | (h::t) -> (f h) + (sumf t f) ;;

(* Test 1 (should return 14): *)
let test1 _ctxt =
  assert_equal (sumf [1;2;3] (fun x -> x*x)) 14 ;;

(* Test 2 (should return 0): *) 
let test2 _ctxt =
  assert_equal (sumf [1;2;-3] (fun x -> x)) 0 ;;

(* Test 3; sum squares of a list of lists (should return true) *) 
let test3 _ctxt =
  assert_equal
    (sumf [[1;2;3];[4;5;6];[7;8;9]] (fun l -> sumf l (fun x -> x*x)))
    (sumf [1;2;3;4;5;6;7;8;9] (fun x -> x*x)) ;; 

(* list of unit tests *)
let unit_tests =
    ["test1">:: test1;
     "test2">:: test2;
     "test3">:: test3
    ];;

(* run the unit tests *)
let () =
  run_test_tt_main
    ("sumf_tests">::: unit_tests);;

(* End of answer to Question M *) 

(*******************************) 
