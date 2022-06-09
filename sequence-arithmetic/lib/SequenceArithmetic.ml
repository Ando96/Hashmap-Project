(* type declaration for intseq *)
type intseq = int list;;


                      (* seqadd *)
(* function that adds two sequences from head to tail, summing at each
   position. *)
let rec seqadd : intseq -> intseq -> intseq =
  fun xs ys ->
  (* Pattern match on both of the lists *)
  match xs,ys with 
  (* If the first list is empty then return a empty list *)
    [] , _  -> []
  (* If the second list is empty then return a empty list *)
  | _ , [] -> []
  (* Split the first elements from both list xs and ys *)
  | (x::xss) , (y::yss) ->
  (* Add the 2 elements together and construct a list by recursivly calling 
  the function again and pass the rest of xs and ys *)
  x + y :: seqadd xss yss;;
 

                          (* seqmult *)
(* function that multiplies two lists from head to tail, multiplying
   at each position *)
let rec seqmult : intseq -> intseq -> intseq =
  fun xs ys ->
  (* Pattern match on both of the lists *)
   match xs,ys with 
  (* If the first list is empty then return an empty list *)
  [] , _  -> []
  (* If the second list is empty return a empty list *)
  | _ , [] -> []
  (* Split the first element from both lists xs and ys *)
  | (x::xss) , (y::yss) ->
  (* Multiply the 2 elements together and construct a new list by 
  recursivly calling the function and passing it the remainder of the lists *)
  x * y :: seqmult xss yss;;