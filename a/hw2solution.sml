(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, xs) =
   case xs of
      [] => NONE
      | x::xs' => if same_string(s, x)
                  then SOME(xs')
                  else case all_except_option(s, xs') of
                        NONE => NONE
                        | SOME(ys) => SOME(x::ys)

fun get_substitutions1(xss, s) =
   case xss of
      [] => []
      | xs::xss' => case all_except_option(s, xs) of
                     NONE => get_substitutions1(xss', s)
                     | SOME(ys) => ys @ get_substitutions1(xss', s)

fun get_substitutions2(xss, a) =
   let fun accumulate(xss, a, acc) = 
      case xss of
      [] => acc
      | xs::xss' => let val substitutions = all_except_option(a, xs)
                     in case substitutions of
                        NONE => accumulate(xss', a, acc)
                        | SOME(ys) => accumulate(xss', a, acc @ ys)
                     end
   in
      accumulate(xss, a, [])
   end

fun similar_names(xss, {first=f, last=l, middle=m}) =
   let 
      val first_name_substitutions = get_substitutions1(xss, f)
      fun replaced_names(subs, acc) = 
         case subs of
         [] => acc
         | sub::subs' => let 
                        in replaced_names(subs', acc @ [{first=sub, last=l, middle=m}])
                        end
   in {first=f, last=l, middle=m}::replaced_names(first_name_substitutions, [])
   end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(c) =
   case c of
   (Spades, _) => Black
   | (Clubs, _) => Black
   | _ => Red

fun card_value(c) =
   case c of
   (_, Num n) => n
   | (_, Ace) => 11
   | _ => 10

fun remove_card(cs, c, ex) =
   case cs of
   [] => raise ex
   | card::cs' => if card = c
                  then cs'
                  else card::remove_card(cs', c, ex)
            
fun all_same_color(cs) =
   case cs of
   [] => true
   | _::[] => true
   | head::(neck::rest) => card_color(head) = card_color(neck) 
                           andalso all_same_color(neck::rest) = true 

fun sum_cards(cs) =
   let fun sum_helper(xs, acc) =
      case xs of
      [] => acc
      | x::xs' => sum_helper(xs', acc + card_value(x)) 
   in
      sum_helper(cs, 0)
   end

fun score(cs, goal) =
   let 
      val sum = sum_cards(cs)
      fun get_prelim_score(sum, goal) = 
         if sum > goal
         then 3 * (sum - goal)
         else goal - sum
   in
      let val prelim_score = get_prelim_score(sum, goal)
      in if all_same_color(cs)
         then prelim_score div 2
         else prelim_score
      end
   end

fun officiate(cs, ms, g) =
   let
     fun helper(cards, moves, helds, goal) =
         case moves of
         [] => score(helds, goal)
         | Discard c::moves' => 
            let val remainings = remove_card(cards, c, IllegalMove)
            in helper(cards, moves', remainings, goal)
            end
         | Draw::moves' =>
            case cards of
            [] => score(helds, goal)
            | c::cards' => 
               if sum_cards(helds) > goal
               then score(helds, goal)
               else helper(cards', moves', c::helds, goal)
   in
     helper(cs, ms, [], g)
   end