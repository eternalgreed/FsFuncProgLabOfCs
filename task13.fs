// 39.1
let rec rmodd = function
    | [] | [_] -> []
    | _ :: x :: tail -> x :: rmodd tail

// 39.2
let rec del_even = function
    | [] -> []
    | head :: tail when head % 2 = 0 -> del_even tail
    | head :: tail -> head :: del_even tail

// 39.3
let rec multiplicity x xs =
    match xs with
    | [] -> 0
    | head :: tail when head = x -> 1 + multiplicity x tail
    | _ :: tail -> multiplicity x tail

// 39.4
let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x :: y :: tail ->
        let xs, ys = split tail
        (x :: xs, y :: ys)

// 39.5
let rec zip (xs1, xs2) =
    match xs1, xs2 with
    | [], [] -> []
    | x :: xs, y :: ys -> (x, y) :: zip (xs, ys)
    | _ -> failwith "Lists must have the same length"
