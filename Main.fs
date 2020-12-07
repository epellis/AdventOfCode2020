module AdventOfCode

open Common
open Days.Day1
open Days.Day2
open Days.Day3
open Days.Day4
open Days.Day5
open Days.Day6

[<EntryPoint>]
let main args =
    match Array.toList args with
    | [ "1"; filename ] ->
        let numbers = Common.fileToIntList filename
        // printfn "Sum is: %d" (Day1.twoSum numbers 2020)
        printfn "Sum is: %d" (Day1.threeSum numbers 2020)
        0
    | [ "2"; filename ] ->
        let lines =
            Common.fileToStringList filename
            |> Seq.map Day2.parse
        // let validated = Seq.map (fun (spec, input) -> Day2.validatePartOne spec input) lines
        let validated =
            Seq.map (fun (spec, input) -> Day2.validatePartTwo spec input) lines

        let count = validated |> Seq.filter id |> Seq.length
        printfn "Valid: %A" count
        0
    | [ "3"; filename ] ->
        let landscape =
            Common.fileToStringList filename
            |> Day3.parseInput

        let velocities =
            [ { Day3.X = 1; Day3.Y = 1 }
              { Day3.X = 3; Day3.Y = 1 }
              { Day3.X = 5; Day3.Y = 1 }
              { Day3.X = 7; Day3.Y = 1 }
              { Day3.X = 1; Day3.Y = 2 } ]

        let scores =
            velocities
            |> Seq.map (Day3.countTrees landscape { Day3.X = 0; Day3.Y = 0 })

        let score = Seq.reduce (*) scores
        printfn "Score: %d" score
        0
    | [ "4"; filename ] ->
        let passportInputs =
            Common.fileToNewlineSeparatedList filename

        let validations =
            passportInputs
            |> Seq.map Day4.run
            |> Seq.filter id
            |> Seq.length

        printfn "Counted %A valid passports." validations

        0
    | [ "5"; filename ] ->
        let codes = Common.fileToStringList filename

        let seats = codes |> Seq.map Day5.findSeat

        let highestSeat = seats |> (Seq.maxBy (fun s -> s.Number))
        printfn "Highest Seat: %A Number %A" highestSeat highestSeat.Number

        let occupiedSeatNumbers =
            seats |> Seq.map (fun s -> s.Number) |> Seq.sort

        let startSeatSearch = Seq.min occupiedSeatNumbers

        let lowestEmptySeat =
            Seq.fold (fun lowest seat -> if lowest = seat then lowest + 1 else lowest) startSeatSearch
                occupiedSeatNumbers

        printfn "Lowest Empty Seat: %A" lowestEmptySeat

        0
    | [ "6"; filename ] ->
        let groupEntries =
            Common.fileToNewlineSeparatedList filename
            |> Seq.map (fun s -> s.Split " ")
            |> Seq.map (Array.toList)
            |> Seq.map (Seq.map (Seq.toList))

        let groupAnswerAnyYes = groupEntries |> Seq.map Day6.anyYes
        let groupAnswerAllYes = groupEntries |> Seq.map Day6.allYes

        let totalAny =
            groupAnswerAnyYes
            |> Seq.map (Set.toSeq >> Seq.length)
            |> Seq.reduce (+)

        let totalAll =
            groupAnswerAllYes
            |> Seq.map (Set.toSeq >> Seq.length)
            |> Seq.reduce (+)

        let printList = Seq.iter (printf "%A ")

        printfn "Total Any: %A" totalAny
        printfn "Total All: %A" totalAll
        0
    | _ -> -1
