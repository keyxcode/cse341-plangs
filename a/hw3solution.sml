(* Coursera Programming Languages, Homework 3, Provided Code *)

(* exception NoAnswer

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
    end *)

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

(* val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x *)

val longest_string_helper = fn condition => fn xs => 
							List.foldl (fn (acc, num) => if condition(String.size(acc), String.size(num)) then num else acc) "" xs

(* val longest_string3 xs = longest_string_helper *)

(* fun longest_string4 xs = List.foldl (fn (acc, num) => if String.size(acc) < String.size(num) then num else acc) "" xs  *)