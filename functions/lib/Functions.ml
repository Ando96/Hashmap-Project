(* A function of many arguments that accepts one arguement 
and returns a function to consume to remaining arguments *)

(* Curry function *)
(* To use the curry function arguments in the form of a tuple must be passed
to another argument, for example:
curry (fun (x,y) -> x + y) 4 3 ;;
which would output 7 *)
let curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c =
   fun f x y -> f (x,y);;



(* uncurry function *)
(* To use the uncurry function pass 
 curry (fun (x,y) -> x + y) 4 3 ;;
 Which outputs the same as the uncurry function, 7. *)
let uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c =
(* Bind function to f, and the arguments to tuple *)
(* Then apply the function to the arguments *)
  fun f (x,y) -> f x y ;;