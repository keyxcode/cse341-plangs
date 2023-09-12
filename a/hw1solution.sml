fun is_older((a: int, b: int, c: int), (x: int, y: int, z: int)) =
    if (a < x) orelse ((a = x) andalso (b < y)) orelse ((a = x) andalso (b = y) andalso (c < z))
    then true
    else false