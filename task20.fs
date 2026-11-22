// 49.5.1
let even_seq =
    Seq.initInfinite (fun i -> 2 * (i + 1))

// 49.5.2
let fac_seq =
    Seq.initInfinite (fun n ->
        let rec factorial current result =
            if current <= 1 then
                result
            else
                factorial (current - 1) (result * current)

        factorial n 1)
    |> Seq.cache

// 49.5.3
let seq_seq =
    Seq.initInfinite (fun i ->
        if i % 2 = 0 then i / 2 else -(i / 2 + 1))
