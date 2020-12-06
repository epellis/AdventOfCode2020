module Common

open System.IO

module Common =
    let fileToStringList filename = File.ReadLines filename |> Seq.toList

    let fileToIntList filename =
        fileToStringList filename |> List.map int

    let fileToNewlineSeparatedList filename =
        let contents = File.ReadAllText filename
        contents.Split("\n\n")
        |> Seq.map (fun s -> s.Replace("\n", " "))
