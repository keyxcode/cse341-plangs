fun is_older(date1: int*int*int, date2: int*int*int) =
    if (#1 date1 < #1 date2) 
        orelse ((#1 date1 = #1 date2) andalso (#2 date1 < #2 date2)) 
        orelse ((#1 date1 = #1 date2) andalso (#2 date1 = #2 date2) andalso ((#3 date1 < #3 date2)))
    then true
    else false


fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
        if month = #2 (hd dates)
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)


fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
        if month = #2 (hd dates)
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)


fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun  get_nth(strings: string list, n: int) = 
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)


fun date_to_string(date: (int*int*int)) =
    let val date_lookups = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(date_lookups, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum(sum: int, nums: int list) = 
    let val sum_minus_head = sum - hd nums
    in 
        if sum <= hd nums
        then 0
        else 1 + number_before_reaching_sum(sum_minus_head, tl nums)
    end


fun what_month(num: int) =
    let val day_lookups = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(num, day_lookups) + 1
    end


fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)


fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else
        let val oldest_tail = oldest(tl dates)
        in
            if isSome oldest_tail
            then
                if is_older(hd dates, valOf oldest_tail)
                then SOME(hd dates)
                else oldest_tail
            else
                SOME(hd dates)
        end


fun dates_are_equal(date1: int*int*int, date2: int*int*int) =
    if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 = #3 date2
    then true
    else false


fun remove_duplicates(dates: (int*int*int) list) =
    if null dates
    then []
    else
        let val unique_tail = remove_duplicates(tl dates)
        in
            if null unique_tail
            then [hd dates]
            else if dates_are_equal(hd dates, hd unique_tail)
            then unique_tail
            else (hd dates) :: (unique_tail)
        end


(* fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    0


fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
    [0] *)