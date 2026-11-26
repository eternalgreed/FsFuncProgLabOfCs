// 50.2.1
let fac_seq =
    seq {
        let mutable factorial = 1
        let mutable n = 1

        while true do
            yield factorial
            factorial <- factorial * n
            n <- n + 1
    }

// 50.2.2
let seq_seq =
    seq {
        yield 0
        let mutable n = 1

        while true do
            yield -n
            yield n
            n <- n + 1
    }
