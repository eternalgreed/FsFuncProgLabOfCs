// 40.1
let rec sum (p: int -> bool, xs: int list) =
    match xs with
    | [] -> 0
    | head :: tail when p head -> head + sum (p, tail)
    | _ :: tail -> sum (p, tail)

// 40.2.1
let rec count (xs: int list, n: int) =
    match xs with
    | [] -> 0
    | head :: _ when head > n -> 0
    | head :: tail when head = n -> 1 + count (tail, n)
    | _ :: tail -> count (tail, n)

// 40.2.2
let rec insert (xs: int list, n: int) =
    match xs with
    | [] -> [n]
    | head :: _ when n <= head -> n :: xs
    | head :: tail -> head :: insert (tail, n)

// 40.2.3
let rec intersect (xs1: int list, xs2: int list) =
    match xs1, xs2 with
    | [], _ | _, [] -> []
    | head1 :: tail1, head2 :: tail2 ->
        if head1 < head2 then
            intersect (tail1, xs2)
        elif head1 > head2 then
            intersect (xs1, tail2)
        else
            head1 :: intersect (tail1, tail2)

// 40.2.4
let rec plus (xs1: int list, xs2: int list) =
    match xs1, xs2 with
    | [], _ -> xs2
    | _, [] -> xs1
    | head1 :: tail1, head2 :: tail2 ->
        if head1 <= head2 then
            head1 :: plus (tail1, xs2)
        else
            head2 :: plus (xs1, tail2)

// 40.2.5
let rec minus (xs1: int list, xs2: int list) =
    match xs1, xs2 with
    | [], _ -> []
    | _, [] -> xs1
    | head1 :: tail1, head2 :: tail2 ->
        if head1 < head2 then
            head1 :: minus (tail1, xs2)
        elif head1 > head2 then
            minus (xs1, tail2)
        else
            minus (tail1, tail2)

// 40.3.1
let rec smallest (xs: int list) =
    match xs with
    | [] -> None
    | head :: tail ->
        match smallest tail with
        | Some value when value < head -> Some value
        | _ -> Some head

// 40.3.2
let rec delete (n: int, xs: int list) =
    match xs with
    | [] -> []
    | head :: tail when head = n -> tail
    | head :: tail -> head :: delete (n, tail)

// 40.3.3
let rec sort xs =
    match smallest xs with
    | None -> []
    | Some value -> value :: sort (delete (value, xs))

// 40.4
let rec revrev xs =
    let rec reverse remaining result =
        match remaining with
        | [] -> result
        | head :: tail -> reverse tail (List.rev head :: result)

    reverse xs []
