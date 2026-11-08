// 42.3
let rec allSubsets n k =
    if k < 0 || k > n then
        Set.empty
    elif k = 0 then
        Set.singleton Set.empty
    elif k = n then
        Set.singleton (set [1 .. n])
    else
        let withoutN = allSubsets (n - 1) k
        let withN =
            allSubsets (n - 1) (k - 1)
            |> Set.map (Set.add n)

        Set.union withoutN withN
