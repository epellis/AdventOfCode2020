module AdventOfCode

open Common
open Days.Day1
open Days.Day2

[<EntryPoint>]
let main args =
    match Array.toList args with
    | ["1"; filename] -> 
        let numbers = Common.fileToIntList filename
        // printfn "Sum is: %d" (Day1.twoSum numbers 2020)
        printfn "Sum is: %d" (Day1.threeSum numbers 2020)
        0
    | ["2"; filename] ->
        let lines = Common.fileToStringList filename |> Seq.map Day2.parse
        let validated = Seq.map (fun (spec, input) -> Day2.validate spec input) lines
        let count = validated |> Seq.filter id |> Seq.length
        printfn "Valid: %A" count
        0
    | _ -> -1
