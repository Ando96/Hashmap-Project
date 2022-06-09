  (* Bubble Sort *)
  let rec bubble_sort (list:'a list) : 'a list =
    let sort = 
      match list with 
      | head1 :: head2 :: rest ->
        if head1 > head2 then 
          head2 :: bubble_sort (head1 :: rest)
        else
          head1 :: bubble_sort (head2 :: rest)
      | rest -> rest
    in
        if list = sort then 
          list
        else
          bubble_sort sort

  (* Filter *)
  let rec filter : ('a -> bool) -> 'a list -> 'a list =
  fun f xs -> 
  match xs with 
  [] -> []
  | (x::rest) -> 
  if (f x) 
  then x :: filter f rest 
  else filter f rest;;

  (* Quick Sort *)
  let rec quick_sort : 'a list -> 'a list = 
    fun list ->
    match list with 
    [] -> []
    |(head :: rest) ->
    let small = filter (fun y-> y <= head) rest
    and large = filter (fun y -> y > head) rest
    in (quick_sort small) @ [head] @ (quick_sort large);;