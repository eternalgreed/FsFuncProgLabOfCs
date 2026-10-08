type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) (x: TimeOfDay) (y: TimeOfDay) =
    let toMinutes time =
        let halfDay = if time.f = "PM" then 12 else 0
        (halfDay + time.hours) * 60 + time.minutes

    toMinutes x > toMinutes y
