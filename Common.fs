module Common

open System.IO

module Common =
    let fileToStringList filename = File.ReadLines filename |> Seq.toList

    let fileToIntList filename =
        fileToStringList filename |> List.map int
