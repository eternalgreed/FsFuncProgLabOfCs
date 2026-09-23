// 16.1
let notDivisible (n, m) = m % n <> 0

// 16.2
let prime n =
    let rec test divisor =
        divisor >= n
        || (notDivisible (divisor, n) && test (divisor + 1))

    n >= 2 && test 2
