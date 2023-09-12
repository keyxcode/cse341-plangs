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
        else 0 + number_in_month(tl dates, month)