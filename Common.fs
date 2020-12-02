module Common

open System.IO

module Common =
    let fileToIntList filename =
        File.ReadLines filename 
        |> Seq.map int
        |> Seq.toList

    let fileToStringList filename =
        File.ReadLines filename 
        |> Seq.toList
