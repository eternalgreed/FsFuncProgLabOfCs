// 23.4.1
let moneyToCopper (gold, silver, copper) =
    gold * 20 * 12 + silver * 12 + copper

let normalizeMoney total =
    let copper = ((total % 12) + 12) % 12
    let totalSilver = (total - copper) / 12
    let silver = ((totalSilver % 20) + 20) % 20
    let gold = (totalSilver - silver) / 20
    (gold, silver, copper)

let (.+.) x y =
    normalizeMoney (moneyToCopper x + moneyToCopper y)

let (.-.) x y =
    normalizeMoney (moneyToCopper x - moneyToCopper y)

// 23.4.2
let (.+) ((a, b): float * float) ((c, d): float * float) =
    (a + c, b + d)

let (.-) ((a, b): float * float) ((c, d): float * float) =
    (a, b) .+ (-c, -d)

let (.*) ((a, b): float * float) ((c, d): float * float) =
    (a * c - b * d, b * c + a * d)

let (./) ((a, b): float * float) ((c, d): float * float) =
    let denominator = c * c + d * d
    (a, b) .* (c / denominator, -d / denominator)
