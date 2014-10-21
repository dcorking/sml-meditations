(* A learning journey towards pattern matching *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except(str, lst) =
   case (str, lst) of
       (_,[]) => []
     | (x,head::tail) => if same_string(x, head)
                         then all_except(x, tail)
                         else head::all_except(x, tail)

val testa = all_except("foo", []) = []
val testb = all_except("foo", ["splat", "foo", "baz"]) = ["splat", "baz"]
val testc = all_except("foo", ["splat", "baz"]) = ["splat", "baz"]
val testd = all_except("foo", ["splat", "foo", "baz", "foo"]) = ["splat", "baz"]

fun all_except_bool(str, lst, matched) =
    case (str, lst, matched) of
        (_,[],matched)         => ([],matched)
      | (x,head::tail,matched) => if same_string(x, head)
                                  then (head::(#1(all_except_bool(x,
                                                                  tail,
                                                                  true)))
                                       , true)
                                  else (head::(#1(all_except_bool(x,
                                                                 tail,
                                                                 matched)))
                                       , #2(all_except_bool(x,
                                                            tail,
                                                            matched)))
(* all_except_bool compiles but tests b and d fail. *)
val testba = all_except_bool("foo", [], false) = ([], false)
val testbb = all_except_bool("foo", ["splat", "foo", "baz"], false) = (["splat", "baz"], true)
val testbc = all_except_bool("foo", ["splat", "baz"], false) = (["splat", "baz"], false)
val testbd = all_except_bool("foo", ["splat", "foo", "baz", "foo"], false) = (["splat", "baz"], true)

(* Finding minimum program to demonstrate
uncaught exception Option
  raised at: smlnj/init/pre-perv.sml:21.28-21.34
*)

fun double pr =
    case pr of
        (0,0) => NONE
         | (a,b) => SOME ( 2*a, 2* b)

fun multiply pr =
    case pr of
        (0,_) => 0
      | (a,b) => b + multiply (a-1,b)

(* This program demonstrates the problem, it compiles, but when I run it
I get
multiply_option(5,4);

uncaught exception Option
  raised at: smlnj/init/pre-perv.sml:21.28-21.34

fun multiply_option pr =
    case pr of
        (0,_) => NONE
      | (a,b) => SOME (b + valOf(multiply_option (a-1,b)))
*)

(* If I omit the parentheses around valOf, I get
 Error: right-hand-side of clause doesn't agree with function result type [circularity]
  expression:  ''Z option option
  result type:  ''Z option
  in declaration:
    multiply_option =
      (fn pr =>
            (case pr
              of <pat> => <exp>
               | <pat> => <exp>))
*)

(* This demonstrates the fix for the uncaught run time exception Option *)
(* I _think_ that the fix is to avoid the occurence of `valOf NONE;` *)
fun multiply_option pr =
    case pr of
        (0,_) => NONE
      | (a,b) => if a = 1
                 then SOME b
                 else SOME (b + valOf(multiply_option(a-1,b)))
val testmo1 = multiply_option(0,4) = NONE
val testmo2 = multiply_option(1,4) = SOME 4
val testmo3 = multiply_option(5,4) = SOME 20

(* use more pattern matching *)
fun multiply_option_pm pr =
    case pr of
        (0,_) => NONE
      | (_,0) => NONE
      | (a,b) => case multiply_option_pm(a-1,b) of
                     SOME p => SOME (b + p)
                   | NONE => SOME b

val testmop1 = multiply_option_pm(0,4) = NONE
val testmop2 = multiply_option_pm(1,4) = SOME 4
val testmop3 = multiply_option_pm(5,4) = SOME 20
val testmop4 = multiply_option_pm(5,0) = NONE

fun inc_or_zero intoption =
    case intoption of
        NONE => 0
      | SOME i => i+1

fun list_or_empty lstoption =
    case lstoption of
         NONE => []
       | SOME a => a

val first_test = list_or_empty NONE = []
val second_test = list_or_empty(SOME ["A"]) = ["A"]


(* The first pattern and its result go right in the first line of the
function binding *)
fun identity ([]) = []
  | identity (hd::tl) = hd::tl

fun identity2 lst =
    case lst of
        [] => []
      | hd::tl => hd::tl

val test_id1 = identity [] = []
val test_id2 = identity [1,2,3] = [1,2,3]
