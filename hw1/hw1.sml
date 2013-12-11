(* code for the hk1. *)

fun is_older(date1 : int*int*int, date2 : int*int*int) = 
    if #1 date1 < #1 date2 then true
    else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2 then true 
    else if #2 date1 = #2 date2 andalso #3 date1 = #3 date2 then true 
    else false

fun number_in_month(date_list : (int*int*int) list, month : int) = 
    if null date_list then 
	0
    else let val x = if #2 (hd date_list) = month then 1 else 0
	 in 
	     x + number_in_month(tl date_list, month)
	 end
fun number_in_months(date_list : (int*int*int) list, months : int list) =
    if null months then
	0
    else number_in_month(date_list, hd months) + 
	 number_in_months(date_list, tl months)

fun dates_in_month(date_list : (int*int*int) list, month : int) =
    if null date_list then []
    else
	if #2 (hd date_list) = month then
	    (hd date_list)::dates_in_month(tl date_list, month)
	else dates_in_month(tl date_list, month)

fun dates_in_months(date_list : (int*int*int) list, months : int list) =
    if null months then []
    else dates_in_month(date_list, hd months)@dates_in_months(date_list, tl months)

fun get_nth(l : string list, n : int) =
    if n = 1 then hd l
    else get_nth(tl l, n-1)

fun date_to_string(d : int*int*int) = 
    let val table = ["January", "February", "March", "April", "May", "June", "July",
		     "August", "September", "October", "November", "December"]
    in
	get_nth(table, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end

fun number_before_reaching_sum(sum:int, l:int list) = 
    let fun helper(cur:int, l:int list) = 
	    if cur + hd l >= sum then 0
	    else 1 + helper(cur + hd l, tl l) in
	helper(0,l)
    end

fun what_month(dnum : int) = 
    let val table = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
	1+ number_before_reaching_sum(dnum, table)
    end

fun month_range(d1 : int, d2:int) = 
    if d1 > d2 then []
    else what_month(d1) :: month_range(d1+1, d2)

fun oldest(dlist : (int*int*int) list) = 
    if null dlist then NONE 
    else 
	let val d2 = oldest(tl dlist) in
	    if isSome d2 andalso is_older(valOf d2, hd dlist)
	    then d2
	    else SOME (hd dlist)
	 end
