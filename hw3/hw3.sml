use "hw3provided.sml";

fun only_capitals (strlist:string list) =
    List.filter (fn s => Char.isUpper(String.sub(s,0))) strlist

fun longest_string1 strlist =
    List.foldl (fn (x,acc) => 
        if String.size x > String.size acc then x else acc)
        "" strlist

(* returning the longest string, in case of tie, return closest of list ending
   could also use List.foldr *)
fun longest_string2 strlist =
    List.foldl (fn (x,acc) => 
        if String.size x >= String.size acc then x else acc)
        "" strlist

fun longest_string_helper f strlist =
    List.foldl (fn (x,acc) => 
        if f (String.size x, String.size acc) then x else acc)
        "" strlist

val longest_string3 = longest_string_helper (fn (a,b) => a > b)
val longest_string4 = longest_string_helper (fn (a,b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

fun rev_string str =
    String.implode (List.rev (String.explode str))

fun first_answer f l =
    case l of
        [] => raise NoAnswer
        | x::xs => case f x of
            SOME v => v
            | NONE => first_answer f xs

fun all_answers f l = 
    let fun helper (acc, lst) =
             case lst of
                  [] => SOME acc
                  | x::xs => case f x of
                      NONE => NONE
                      | SOME v => helper(acc @ v, xs) 
    in 
        helper([], l)
    end

(*fun count_wildchars pattern =  g (fn () => 1) (fn (s) => 0) pattern() 
    二者是等效的*)
val count_wildcards = g (fn () => 1) (fn (s) => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn (s) => String.size s)
fun count_some_var (str, ptr) = 
    g (fn () => 0) (fn (s) => if s = str then 1 else 0) ptr

fun check_pat pat =
    let fun get_str_list p0 =
            case p0 of
                Variable x        => [x]
              | TupleP ps         => List.foldl (fn (p,acc) =>  acc @ (get_str_list p)) [] ps
              | ConstructorP(_,p) => get_str_list p
              | _                 => []
        fun all_distinct (strlist, acc) = 
            case strlist of
                [] => true
                | x::xs => if List.exists (fn (elem) => elem = x) acc then false 
                        else  all_distinct(xs, x::acc)
    in
        all_distinct(get_str_list pat, [])
    end

(* nested case pattern matching, 两个一起来, 不要写两层case epression *)
fun match (valu, pat) =
    case (valu,pat) of
          (v, Variable s) => SOME [(s,v)]
        | (Unit, UnitP) => SOME []
        | (Const v, ConstP s) => if s = v then SOME [] else NONE
        | (Tuple t, TupleP pt) => all_answers match (ListPair.zip(t,pt))
        | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v,p) else NONE
        | (_, Wildcard) => SOME []
        | _ => NONE

fun first_match valu patlist =
    SOME (first_answer match (List.map (fn (pat) => (valu, pat)) patlist))
    handle NoAnswer => NONE





