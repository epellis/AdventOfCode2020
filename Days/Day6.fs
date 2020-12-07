module Days.Day6

module Day6 =
    let emptyAnswers: Set<char> = Set.empty

    let addToSet set entry = Set.union set (Set.ofSeq entry)

    let takeFromSet set entry = Set.intersect set (Set.ofSeq entry)

    let anyYes (entries: char list seq) = Seq.fold addToSet emptyAnswers entries

    let allYes (entries: char list seq) =
        Seq.fold takeFromSet (Set.ofSeq (Seq.head entries)) (Seq.tail entries)
