(* Naive implementation of append *)
(* concatenate 2 lists of dates *)
fun concat(a : (int*int*int) list, b : (int*int*int) list) =
    if null a
    then b
    else hd a :: concat(tl a, b)

val test_concat_mt = concat([],[(1,2,3),(4,5,6)]) = [(1,2,3),(4,5,6)]
val test_concat_two = concat([(7,8,9),(10,11,12)],[(1,2,3),(4,5,6)]) = [(7,8,9),(10,11,12),(1,2,3),(4,5,6)]
