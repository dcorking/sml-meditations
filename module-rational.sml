(* - (Rational1.make_frac(21,6)); *)
(* val it = Frac (7,2) : Rational1.rational *)


(* Adapted from code by Dan Grossman *)

signature RATIONAL_A =
sig
    datatype rational = Whole of int | Frac of int*int
    exception BadFrac
    val make_frac : int*int -> rational
    (* val add : Rational1.rational*Rational1.rational -> Rational1.rational *)
    (* val toString : Rational1.rational -> string *)
end

structure Rational1 :> RATIONAL_A =
struct
datatype rational = Whole of int | Frac of int*int
exception BadFrac

(* gcd and reduce are not intended for clients *)
fun gcd (x,y) =
    if x=y
    then x
    else if x < y
    then gcd(x,y-x)
    else gcd(y,x)

fun reduce r =
    case r of
        Whole _ => r
     | Frac(x,y) =>
       if x=0
       then Whole 0
       else let val d = gcd(abs x,y) in (* enforces denom > 0 *)
                if d=y
                then Whole(x div d)
                else Frac(x div d, y div d)
            end

(* make frac only with non-zero denoms *)
fun make_frac (x,y) =
    if y=0
    then raise BadFrac
    else if y < 0
    then reduce(Frac(~x, ~y))
    else reduce(Frac(x,y))

fun add (r1,r2) =
    case (r1,r2) of
        (Whole(i), Whole(j)) => Whole(i+j)
      | (Whole(i), Frac(j,k)) => Frac(j+k*i,k)
      | (Frac(j,k), Whole(i)) => Frac(j+k*i,k)
      | (Frac(a,b), Frac(c,d)) => reduce (Frac(a*d + b*c, b*d))

(*  this doesn't print in reduced form - despite the so-called
invariant - it doesn't seem to call
 make_frac *)
fun toString r =
    case r of
        Whole i => Int.toString i
      | Frac(a,b) => (Int.toString a) ^ "/" ^ (Int.toString b)

end
