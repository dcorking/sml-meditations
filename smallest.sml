fun smallest(xs : int list) =
    if null (tl xs)
    then hd xs
    else let val tl_result = smallest(tl xs)
         in
             if hd xs < tl_result
             then hd xs
             else tl_result
         end

val test1 = smallest([1,2,3]) = 1
val test2 = smallest([3,2,1]) = 1
(* val test3 = smallest([]) = NONE *)
val test4 = smallest([2]) = 2
