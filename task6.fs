// 17.1
let rec pow (s, n) =
    if n <= 0 then
        ""
    else
        s + pow (s, n - 1)

// 17.2
let rec isIthChar (s, n, c) =
    n >= 0 && n < String.length s && s.[n] = c

// 17.3
let rec occFromIth (s, n, c) =
    if n < 0 || n >= String.length s then
        0
    elif isIthChar (s, n, c) then
        1 + occFromIth (s, n + 1, c)
    else
        occFromIth (s, n + 1, c)
