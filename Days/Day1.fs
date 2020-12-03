module Days.Day1

module Day1 =
    let rec twoSumInner forward reverse sum =
        match (forward, reverse) with
        | ([], _)
        | (_, []) -> None
        | (a :: _, b :: _) when a + b = sum -> Some(a * b)
        | (a :: _, b :: _) when a + b < sum -> twoSumInner forward.Tail reverse sum
        | (a :: _, b :: _) when a + b > sum -> twoSumInner forward reverse.Tail sum
        | _ -> None

    let twoSum xs sum =
        let sorted = List.sort xs
        let reverseSorted = List.rev sorted
        (twoSumInner sorted reverseSorted sum).Value

    let rec threeSumInner xs sum =
        match xs with
        | [] -> None
        | a :: tl ->
            let newSum = sum - a
            match twoSumInner tl (List.rev tl) newSum with
            | Some answer -> Some(a * answer)
            | None -> threeSumInner tl sum

    let threeSum xs sum =
        let sorted = List.sort xs
        (threeSumInner sorted sum).Value
