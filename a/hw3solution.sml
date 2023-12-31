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

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

val longest_string1 = List.foldl (fn (str, acc) => if String.size(str) > String.size(acc)  then str else acc) "" 

val longest_string2 = List.foldl (fn (str, acc) => if String.size(str) >= String.size(acc)  then str else acc) "" 

fun longest_string_helper condition = 
	List.foldl (fn (str, acc) => if condition(String.size(str), String.size(acc)) then str else acc) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = (longest_string3 o only_capitals)

val rev_string = (String.implode o List.rev o String.explode)

fun first_answer f xs =
	case xs of
	[] => raise NoAnswer
	| x::xs' => case f x of
				SOME v => v
				| NONE => first_answer f xs'

fun all_answers f xs =
	let fun helper ys acc =
		case (ys, acc) of
		([], _) => acc
		| (y::ys', SOME v) => (case f y of
								SOME t => helper ys' (SOME (v @ t))
								| NONE => NONE)
		| _ => NONE
	in helper xs (SOME [])
	end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

val count_some_var = fn (s, p) => g (fn _ => 0) (fn x => if String.isSubstring x s then 1 else 0) p

fun check_pat p = 
	let
		fun strings_in_pattern pattern =
		case pattern of
		Variable x => [x]
		| TupleP ps => List.foldl (fn (p, acc) => acc @ (strings_in_pattern p) ) [] ps
		| ConstructorP (s, p) => strings_in_pattern p
		| _ => []

		fun is_distinct xs =
		case xs of
		[] => true
		| x :: xs' => if List.exists (fn item => item = x) xs'
						then false
						else is_distinct xs'
	in is_distinct (strings_in_pattern p)
	end

fun match (v, p) =
case (v, p) of
(_, Wildcard) => SOME []
| (_, Variable s) => SOME [(s, v)]
| (Unit, UnitP) => SOME []
| (Const a, ConstP b) => if a = b then SOME [] else NONE
| (Tuple vs, TupleP ps) => if List.length vs = List.length ps 
						then all_answers match (ListPair.zip (vs, ps))
						else NONE
| (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 then match(v, p) else NONE
| _ => NONE

fun first_match v ps = 
SOME (first_answer (fn p => match (v, p)) ps)
handle NoAnswer => NONE
