(* i2c recursively applies a function f to a parameter x, n times *)
let rec i2c n f x =
    match n with 
    (* if n = 0 then output x *)
    0 -> x 
    | n -> i2c (n-1) f (f x);;
 
(* passes the incrementing function and 0 into the Church numeral, so
   every application of f increases the output by 1. Since it starts
   at 0, this just returns the amount of times f was applied to x,
   thus telling us which Church numeral it was. *)
let c2i x =
    i2c x (fun x -> x+1) 0;;
    
    
    (*   
    
    Alternative way of declaring function, this way means c2i would have to be 
    passed as an arguement when calling the function.
    let c2i x =
  x ((+) 1 ) 0 ;;
    *)