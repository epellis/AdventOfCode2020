module AdventOfCode

open Common
open Days.Day1

[<EntryPoint>]
let main args =
    match Array.toList args with
    | ["1"; filename] -> 
        let numbers = Common.fileToIntList filename
        // printfn "Sum is: %d" (Day1.twoSum numbers 2020)
        printfn "Sum is: %d" (Day1.threeSum numbers 2020)
        0
    | _ -> -1
