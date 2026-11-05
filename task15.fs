// 41.4.1
let list_filter f xs =
    List.foldBack (fun x result -> if f x then x :: result else result) xs []

// 41.4.2
let sum (p, xs) =
    List.fold (fun result x -> if p x then result + x else result) 0 xs

// 41.4.3
let revrev xs =
    List.fold (fun result inner ->
        let reversed = List.fold (fun acc x -> x :: acc) [] inner
        reversed :: result) [] xs
