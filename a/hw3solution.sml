(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0	
    end

(**** for the challenge problem only ****)

(* datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string *)

(**** you can put all your code here ****)

fun only_capitals xs = List.filter (fn s => Char.isUpper(String.sub(s, 0))) xs

fun longest_string1 xs = List.foldl (fn (str, acc) => if String.size(str) > String.size(acc)  then str else acc) "" xs 

fun longest_string2 xs = List.foldl (fn (str, acc) => if String.size(str) >= String.size(acc)  then str else acc) "" xs 

fun longest_string_helper condition xs = 
	List.foldl (fn (str, acc) => if condition(String.size(str), String.size(acc)) then str else acc) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = (longest_string3 o only_capitals)

val rev_string = (String.implode o List.rev o String.explode)

fun first_answer process xs =
	case xs of
	[] => raise NoAnswer
	| x::xs' => case process x of
				SOME v => v
				| NONE => first_answer process xs'

fun all_answers process xs =
	let fun helper ys acc =
		case (ys, acc) of
		([], _) => acc
		| (y::ys', SOME v) => (case process y of
								SOME t => helper ys' (SOME (v @ t))
								| NONE => NONE)
		| _ => NONE
	in helper xs (SOME [])
	end

val count_wildcards = g (fn _ => 1) (fn _ => 0)