// 34.1
let rec upto n =
    let rec build current result =
        if current <= 0 then
            result
        else
            build (current - 1) (current :: result)

    build n []

// 34.2
let rec dnto n =
    if n <= 0 then
        []
    else
        n :: dnto (n - 1)

// 34.3
let rec evenn n =
    let rec build count result =
        if count <= 0 then
            result
        else
            build (count - 1) ((2 * (count - 1)) :: result)

    build n []
