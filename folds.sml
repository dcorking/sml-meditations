(* Folding applies a function to a list element, paired with an
accumulator. The element and the accumulator do *not* have to be the
same type, so that the accumulator can be another list. The function returns the next value of the accumulator *)

(* Here I am trying to make a list of all the elements greater than 3 *)
fun keepbig (elt, accum) = if elt > 3 then [elt] @ accum else accum
(* val keepbig = fn : int * int list -> int list *)

(* In ML, fold is curried, and its first argument is the folding
function, second is the accumulator (an identity member, or an empty
list to build lists), and the third is list input *)
(*
- foldr;
val it = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
 *)
val test1 = foldr keepbig [] [2,3,4,5,6] = [4,5,6] (* build a list from the right *)
val test2 = foldl keepbig [] [2,3,4,5,6] = [6,5,4] (* build from the left *)

(* Uses a predicate function to filter a list using fold, in other
 words an overcomplicated way to define filter *)
fun all_passing predicate lst =
    let fun build (elt, answers) =
            case predicate elt of
                false => answers
              | true => [elt] @ answers
    in
        foldr build [] lst
    end
(* val all_passing = fn : ('a -> bool) -> 'a list -> 'a list *)
val test_ap1 = all_passing (fn a => a > 3) [1,5,3,6] = [5,6]
