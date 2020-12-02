module Days.Day2

open FParsec

module Day2 =
    type PasswordSpec = {
        MinCount : int
        MaxCount : int
        Letter : char
    }

    let numberParser = pint32 .>> spaces
    let rangeParser = numberParser .>> skipChar '-' .>>. numberParser .>> spaces
    let letterParser = anyChar .>> skipChar ':' .>> spaces
    let stringParser = manyChars letter .>> spaces

    let parser = rangeParser .>>. letterParser .>>. stringParser 

    exception ParsingException of string

    let parse input =
        match run parser input with
        | Success (result, _, _) -> 
            let (((low, hi), letter), input) = result
            { MinCount = low; MaxCount = hi; Letter = letter}, input
        | Failure (error, _, _) -> 
            raise (ParsingException error)
            
    let validatePartOne spec text =
        let count = (Seq.filter ((=) spec.Letter) text) |> Seq.length
        (spec.MinCount <= count) && (count <= spec.MaxCount)

    let validatePartTwo spec text =
        let count = (Seq.filter ((=) spec.Letter) text) |> Seq.length
        (spec.MinCount <= count) && (count <= spec.MaxCount)
