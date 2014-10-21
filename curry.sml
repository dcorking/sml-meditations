(* Curry binary multiplication *)

fun curry f a b =
    f(a,b)
(* val curry = fn : ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
(* curry has one argument, f, which is a function that takes
   a pair argument. Its output is a curried function that takes
   one argument, partially applies it, returning another function
   that works on another argument.

   How do beginners read code that depends on currying, especially in
   JavaScript? *)

val mul = curry (op * )
(* val mul = fn : int -> int -> int *)
(* mul is a curried function, that can be used to multiply 2 ints, by
   repeated function application *)

val test1 = mul 5 6 = 30
val test2 = (mul 4) 9 = 36

(* uncurries 2 repeated function applications to become a function on a pair *)
fun uncurry f(a,b) =
    f a b
(* val uncurry = fn : ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)

(* uncurry our curried mul, to get the answer to the ultimate question *)
val test3 = uncurry mul (6,7) = 42;
(* - uncurry mul; *)
(* val it = fn : int * int -> int *)
