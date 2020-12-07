module Days.Day5


module Day5 =
    type Seat =
        { Row: int32
          Column: int32 }

        member this.Number = (this.Row * 8) + this.Column

    let charToBit =
        Map [ ('F', 0b0)
              ('B', 0b1)
              ('L', 0b0)
              ('R', 0b1) ]

    let parseBinary stream =
        let rec parseBinaryRec stream pow acc =
            match stream with
            | [] -> acc
            | head :: tl -> parseBinaryRec tl (pow * 2) acc + (pow * head)

        parseBinaryRec (List.rev stream) 1 0

    let findSeat input =
        let binaryInput =
            input
            |> Seq.toList
            |> List.map (fun c -> charToBit.[c])

        let row = parseBinary binaryInput.[0..6]
        let col = parseBinary binaryInput.[7..9]

        { Row = row; Column = col }
