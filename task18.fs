// 47.4.1
let f n =
    if n < 0 then
        invalidArg "n" "n must be non-negative"

    let mutable result = 1
    let mutable current = n

    while current > 1 do
        result <- result * current
        current <- current - 1

    result

// 47.4.2
let fibo n =
    if n < 0 then
        invalidArg "n" "n must be non-negative"

    let mutable previous = 0
    let mutable current = 1
    let mutable index = 1

    while index < n do
        let next = previous + current
        previous <- current
        current <- next
        index <- index + 1

    if n = 0 then 0 else current
