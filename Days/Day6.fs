module Days.Day6

module Day6 =
    let emptyAnswers: Set<char> = Set.empty

    let addToSet set entry =
        Seq.fold (fun s e -> Set.add e s) set entry

    let buildAnswers (entries: char list seq) = Seq.fold addToSet emptyAnswers entries
