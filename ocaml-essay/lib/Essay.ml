(*                                     --OCaml Essay--

    Function type signatures provide information on what the function does and allow you to understand
    what the purpose of the function may be without running it. 
    Some type signatures are simple to understand such as addition.

    This add function takes 2 arguements of type int (known as the domain) and returns a type int 
    (known as the range). If the name of the function was removed and was simply called 'Fun1', 
    by reading the type signature you could still understand the process of the function, 
    takes 2 numbers and outputs another number.
*)
    let add : int -> int -> int = 
    fun x y -> x + y;;
(*
    If you were to type the name of the function into the terminal you are provided with the type
    signature of the function.  

    val add : int -> int -> int = <fun> 

    Type signatures like this are known as static, meaning that this function can only accpet arguements of 
    type int, providing anything other than that will provide an error, for example attempting to give the add
    function '1' "Hello".
    
   Type signatures do not all have to be the same type however, isEven takes an input of type int and 
   returns an output of type bool.
*)
   let isEven : int -> bool =
   fun i -> (i mod 2) = 0;;  
(*
   val is_even : int -> bool = <fun>

 * - Polymorphism. 
    OCaml provides type variables that allow you to express unknown types within a function, which
    is another way of saying a function that can accept any type aslong as it is uniformly. This is very
    useful in removing the need to rewrite the same code to accept different types of inputs and means 
    only 1 polymorphic function is required.

    A very simple implementation of a polymorphic functions is given below, it accepts any type of input
    and will output the same type. 1 will output 1, "Hello" will output "Hello".

    let output : 'a -> 'a = 
    fun x -> x ;;

    A slightly more complicated example would be a function which returns a list in reversed order,
    the type signature of the list and output are type variables and so the function could accept lists of
    different types for example,

   a list of type int [1;2;3;4;5]
   would return : int list = [5; 4; 3; 2; 1]

   a list of type bool [true;false;true;false]
   would return : bool list = [false; true; false; true]

   a list of type String ["Hello";"Goodbye"]
   would return : string list = ["Goodbye"; "Hello"]
*)
   let rec reverse_list : 'a list -> 'a list =
   fun xs -> match xs with 
   [] -> []
   |(x::rest) -> 
   reverse_list rest @ [x];;

(* Polymorphic functions are best used in functions such as apply, which can accept a function and a value
   and applys the function to the value. *)

   let apply : ('a -> 'b) -> 'a -> 'b =
   fun f x -> f x ;;
(*

 * - List types and tuple types (and their differences). 

Lists are a sequence of values of the same type, a list of type int cannot contain Strings, 
a list of type bool cannot contain floats and soforth. The type of the list can be specifed in 
the type signature to indicate the type the list must be. *)

(* A funtion that takes a list of type int and checks for even numbers *)
let rec any_evens : int list -> bool =
  fun xs -> match xs with
  [] -> false
 | (x::rest) -> 
  if (isEven x)
  then true
  else any_evens rest;; 

(* Lists are also polymorphic and can be used in functions to accept lists of any type meaning the type
of the list does not have to be specified such as in the following example, Length. *)

let rec length : 'a list -> int =
fun xs ->
match xs with
[] -> 0
| (x::rest) -> 1 + length rest;;

(* The function length accepts a list of any type 'a and returns a type int, this function can be given 
a list of type String, a list of type int, a list of type bool and will return the length of it. 

A tuple is a collection that can contain any number of arguments seperated by commas, unlike lists 
a tuple can contain values of different types, for example 

(3,"Hello",7.3) ;;

Will return the type of the tuple,
: int * string * float = (3, "Hello", 7.3)

The type of the tuple is defined by the * symbol, in the above example the type of the tuple is 
first type int * second type string * third type float.

 * - OCaml pattern-matching on values (e.g. integers) and structures (e.g. lists). 
Pattern matching is very similar to switch statements in java and allows you to match on specific values
or situations in functions and handle them accordingly. Its possible to pattern match on several different 
elements such as numbers or lists and handle the flow of the program depending on the content of these values. 

For example:

any_evens tries to match the input list of type int with an empty list, if this is true then the function
will return false, if the list is not empty then the list is split head and rest. This helps with error 
handling as you don't want empty lists passed to the function causing errors.

*)

let rec any_evens : int list -> bool =
  fun xs -> match xs with
  [] -> false
 | (x::rest) -> 
  if (isEven x)
  then true
  else any_evens rest;;

(*
  It is also possible to match on explicit types such as Person, the age_of function accepts a name of 
  a person and then attempts to match that name with a already defined Person returning their age.
*)

  type person = Person of string * int ;;
  let jon = Person ("Jon",45) ;;
  let sarah = Person ("Sarah",23) ;;

  let age_of : person -> int =
  fun person -> 
  match person with Person(name,age) -> age ;;

(*
 * - Named and anonymous functions. 
    
    A named function is a defined function using the keyword let with a type signature 
    and can be called within other functions. If something is going to be required to multiple times
    throught a program it only makes sense to create a function to make life easier. *)

    let double (x:int) : int = x * 2;;

(*
    An anonymous function is a function without a name created by using the fun expression, which does not
    have a declared type signature and is primarly for single use and is only required
    for a single purpose,  for example;

    (fun x -> x + 1);;

    Is an anonymous function which adds 1 to x, oCaml is able to imferr what the type of the function is
    - : int -> int = <fun>

 * - Recursive functions. 
 A recursive function is one that calls itself until some event causes it to stop, such as meeting the 
 requirements of an if statement, or a list running out of values to check. This is done by calling 
 the function name within the function itself and providing it with the arguements it requires.*)

(* replicate_elem is a recursive function (indicated by the rec after let) that takes a counter 
 n and a list, it then recursivly replicates the value in the list until n is = 0, it does this by 
 taking 1 from n and then passing the new value to the function and calling it again. *)
 let rec replicate_elem : int -> 'a -> 'a =
  fun n xs -> match xs with 
  [] -> []
  |(xs) -> 
  if(n > 0)
  then replicate_elem (n-1) xs@xs
  else [];;

(*

 * - Unit and property based tests. 

 Unit tests are used to validate that a specific 'function or component' is working as intended, in OCaml
 we can use unit testing with functions that have been implemented. The assert_equals function is used 
 to ensure that the expected outcome is equal to the ouput provided by the function.
 If the function fails to equal what the expected output is the test will fail. 
 
 let add_test_positive _ctxt = 
   assest_equal 
   3
   (add 2 1);;
   (unit tests have been added in EssayTests.ml)

 Property based tests are more indepth and ensure that the function being tested abides by the aspects
 of a property,for example a property based test for addition could be that no matter what order 
 numbers are added the ouput should remain the same or a zero based property test, a number multiplied
 by zero should always be zero. Using property tests you can generate a whole range of possible inputs 
 for the function to cover all the possibilities the function may have to handle or ones you have not 
 thought about yet.
 
 There are a number of different property tests:

 Associative Addition - The order in which numbers are calculated has no effect on the final answer
                        (1 + 5 + 10) + 4 = 20 , (4 + 5) + 10 + 1 = 20;

 Commutative Addition - The order of numbers in addition has no effect on the final answer 
                        1 + 15 = 16 , 15 + 1 = 16;

 Zero Addition - Addition of zero to a number has no effect.
                  1 + 0 = 0 , 15 + 0 = 15 ;
 
 Zero Multiplication - Multiplied by zero always results in 0 
                      1 * 0 = 0 , 1512359 * 0 = 0 ;
 *)