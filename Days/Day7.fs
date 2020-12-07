module Days.Day7

module Day7 =
    type Color = Color of string

    type BagCapacity = Set<int32 * Color>

    module Parser =
        open FParsec

    //     let bagLiteralParser = (skipString "bags" <|> skipString "bag")

    //     let bagParser =
    //         stringsSepBy (manyChars letter) (pstring " ")
    //         .>> bagLiteralParser

    //     let amountBagParser = pint32 .>> spaces1 .>>. bagParser

    //     let lineParser =
    //         bagParser
    //         .>> skipString "contain "
    //         .>>. sepBy amountBagParser (pstring ", ")
    //         .>> skipString "."

    //     let parse input =
    //         printfn "Result: %A" (run lineParser input)

    // // match run lineParser input with
    // // | Success (result, _, _) -> Some result
    // // | Failure (why, _, _) -> None

    let determineCapacity input =
        Parser.parse input
        "", Set.empty
