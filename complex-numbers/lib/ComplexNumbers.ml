type complex_number = CI of int*int;;

(* addition of two Complex Integers *)
(* Following the formula for complex number addition
(a+bi) + (c+di) = (a+c) + (b+d)i *)
let cadd (CI(x,y)) (CI(x2,y2)) =       
CI(x+x2,y+y2);;

(* multiplication of two Complex Integers *)
(* Following the formula for complex number multiplying 
(a+bi)(c+di) = ac + adi + bci + bdi2 *)
let cmult (CI(x,y)) (CI(x2,y2)) =
CI((x*x2) - (y*y2) , (y*x2) + (x*y2));;