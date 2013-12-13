use "hw2provided.sml";

fun all_except_option (s, lst) = 
    case lst of 
	[] => NONE
     |  x::xl  => if same_string(x, s) then SOME xl else
		  case all_except_option(s, xl) of 
		      NONE => NONE
		   |  SOME t  => SOME (x::t)

fun get_substitutions1 (ll, s) = 
    case ll of 
	[] => []
     |  x::xs => case all_except_option(s, x) of
		     NONE => get_substitutions1(xs, s)
		  |  SOME t => t @ get_substitutions1(xs,s)

fun get_substitutions2 (ll,s) =
    let fun helper (acc, hl) = 
	    case hl of 
		[] => acc
	     |  x::xs  => case all_except_option(s,x) of
			      NONE => helper(acc, xs)
			   |  SOME t => helper(acc @ t, xs)
    in helper([], ll)
    end

fun similar_names (sub, {first=f, middle=m, last=l}) =
    let fun helper ll = 
	    case ll of 
		[] => []
	     |  x::xs => {first=x, middle=m, last=l}::helper  xs
    in
	{first=f, middle=m, last=l}::helper(get_substitutions1(sub, f))
    end

datatype suit = Spades | Clubs | Diamonds | Hearts
datatype rank = Jack | Queen | King | Ace |  Num of int
type card = suit * rank
datatype move = Draw | Discard of card

fun  card_color(c:card) =
     case c of 
	 (Spades,_) => Black
      |  (Clubs,_)  => Black
      |  (Diamonds,_)  => Red
      |  (Hearts,_) => Red

(*could sepcify type in type matching *)
fun card_value card = 
    case card of 
	(_:suit, Num(x)) => x
     |  (_:suit, Ace) => 11
     |  (_:suit, _) => 10

fun remove_card (clist:card list, c:card, e:exn) = 
    let fun helper (succ, acc, l) = 
	    case l of 
		[] => if succ then acc else raise e
	     | x::xs  =>  if x = c then helper(true, acc, xs) 
			  else helper(succ, x::acc, xs)
    in
	rev (helper(false, [], clist)) (*rev l - revse list, for efficiency*)
    end

fun all_same_color (cl:card list) = 
    case cl of 
	[] => true
     |  _::[]  => true
     | x::(y::ll) => card_color(x) = card_color(y) andalso all_same_color(ll)

fun sum_cards (cl:card list) = 
    let fun helper (acc:int, l:card list) = 
	    case l of 
		[] => acc
	     |  x::xs => helper(card_value(x) + acc, xs)
    in helper(0, cl)
    end

fun score (cl:card list, goal:int) = 
  let
    val sum = sum_cards(cl)
     val pres = if sum > goal then  3 * (sum - goal)
                else goal - sum
  in
    if all_same_color(cl) then
      pres div 2
    else pres
  end

exception IllegalMove

fun officiate (card_list, move_list, goal) =
    let 
        fun helper(hand, left, ml) =
            case ml of
                [] => score(hand,goal)
                | Discard(dis)::smove =>  
                    helper(remove_card(hand, dis, IllegalMove), left, smove)
                | Draw::smove  => 
                    case left of
                        [] => score(hand,goal)
                        | cc::sleft => 
                            if sum_cards(cc::hand) > goal then score(cc::hand, goal)
                            else    helper(cc::hand, sleft, smove)
    in
        helper([], card_list, move_list)
    end

