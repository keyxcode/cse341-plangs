use "hw3solution.sml";

(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["A", "bc", "cd", "C"] = "bc"
val test2c = longest_string1 ["A", "ab", "bc", "cd", "C"] = "ab"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 ["A","bc", "cd", "C"] = "cd"
val test3c = longest_string2 ["A", "ab", "bc", "cd", "C"] = "cd"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string3 ["A", "bc", "cd", "C"] = "bc"
val test4c = longest_string3 ["A", "ab", "bc", "cd", "C"] = "ab"

val test4d = longest_string4 ["A","B","C"] = "C"
val test4e = longest_string4 ["A","bc", "cd", "C"] = "cd"
val test4f = longest_string4 ["A", "ab", "bc", "cd", "C"] = "cd"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) = 3

val test9b = count_wild_and_variable_lengths (Variable("ab cd")) = 5

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c2 = count_some_var ("xyz", TupleP [Variable("xyz"), Variable("abc"), Variable("xyz")]) = 2

val test10 = check_pat (Variable("x")) = true
val test10b = check_pat (ConstructorP ("SOME", Variable "x")) = true
val test10c = check_pat (ConstructorP ("SOME", TupleP [Variable "x", ConstP 3])) = true
val test10d = check_pat (ConstructorP ("SOME", TupleP [Variable "x", Variable "y", ConstP 3])) = true
val test10e = check_pat (ConstructorP ("SOME", TupleP [Variable "x", Variable "x", ConstP 3])) = false

(* val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME [] *)