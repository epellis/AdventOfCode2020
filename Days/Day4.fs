module Days.Day4


module Day4 =
    type Height =
        | Inches of int32
        | Centimeters of int32

    type HexColor = HexColor of string
    type PassportEncoding = PassportEncoding of string

    type FieldNode =
        | BirthYear of int32
        | IssueYear of int32
        | ExpYear of int32
        | Height of Height
        | HairColor of HexColor
        | EyeColor of string
        | PassportId of PassportEncoding
        | CountryId of string

    module Parser =
        open FParsec

        let spaceParser = (spaces1 <|> eof)

        let stringParser prefix =
            skipString prefix
            >>. manyCharsTill anyChar spaceParser

        let intParser prefix =
            skipString prefix >>. pint32 .>> spaceParser

        let inParser = pint32 .>> skipString "in" |>> Inches

        let cmParser =
            pint32 .>> skipString "cm" |>> Centimeters

        let heightParser prefix =
            skipString prefix
            >>. (attempt inParser <|> attempt cmParser)
            .>> spaceParser

        let hexColorParser prefix =
            skipString prefix
            >>. skipChar '#'
            >>. manyMinMaxSatisfy 6 6 isHex
            |>> HexColor
            .>> spaceParser

        let passportIdParser prefix =
            skipString prefix
            >>. manyMinMaxSatisfy 9 9 isDigit
            |>> PassportEncoding
            .>> spaceParser

        let parseBirthYear = (intParser "byr:") |>> BirthYear
        let parseIssueYear = (intParser "iyr:") |>> IssueYear
        let parseExpYear = (intParser "eyr:") |>> ExpYear
        let parseHeight = (heightParser "hgt:") |>> Height
        let parseHairColor = (hexColorParser "hcl:") |>> HairColor
        let parseEyeColor = (stringParser "ecl:") |>> EyeColor
        let parsePassportId = (passportIdParser "pid:") |>> PassportId
        let parseCountryId = (stringParser "cid:") |>> CountryId

        let fieldLiteral =
            choice [ parseBirthYear
                     parseIssueYear
                     parseExpYear
                     parseHeight
                     parseHairColor
                     parseEyeColor
                     parsePassportId
                     parseCountryId ]

        let fieldParser = many fieldLiteral

        let parse input =
            match run fieldParser input with
            | Success (result, _, _) -> Some result
            | Failure (why, _, _) -> None

    let between lower value upper = (lower <= value) && (value <= upper)

    let validHairColors =
        Set [ "amb"
              "blu"
              "brn"
              "gry"
              "grn"
              "hzl"
              "oth" ]

    let validate field =
        match field with
        | BirthYear year -> between 1910 year 2002
        | IssueYear year -> between 2010 year 2020
        | ExpYear year -> between 2020 year 2030
        | Height (Inches height) -> between 59 height 76
        | Height (Centimeters height) -> between 150 height 193
        | HairColor (HexColor _) -> true
        | EyeColor color -> validHairColors.Contains color
        | PassportId (PassportEncoding _) -> true
        | CountryId _ -> true

    let validateAndMark field (state: Map<string, bool>) =
        let isValid = validate field
        match field with
        | BirthYear _ -> Map.add "BirthYear" (isValid || state.["BirthYear"]) state
        | IssueYear _ -> Map.add "IssueYear" (isValid || state.["IssueYear"]) state
        | ExpYear _ -> Map.add "ExpYear" (isValid || state.["ExpYear"]) state
        | Height _ -> Map.add "Height" (isValid || state.["Height"]) state
        | HairColor _ -> Map.add "HairColor" (isValid || state.["HairColor"]) state
        | EyeColor _ -> Map.add "EyeColor" (isValid || state.["EyeColor"]) state
        | PassportId _ -> Map.add "PassportId" (isValid || state.["PassportId"]) state
        | CountryId _ -> Map.add "CountryId" (isValid || state.["CountryId"]) state

    let validateAll fields =
        printfn "Validating %A" fields

        let mutable validatedFields =
            Map [ ("BirthYear", false)
                  ("IssueYear", false)
                  ("ExpYear", false)
                  ("Height", false)
                  ("HairColor", false)
                  ("EyeColor", false)
                  ("PassportId", false)
                  ("CountryId", true) ]

        for field in fields do
            validatedFields <- validateAndMark field validatedFields

        validatedFields
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter id
        |> Seq.length
        |> (=) ((Map.toSeq >> Seq.length) validatedFields)

    let run input =
        Parser.parse input
        |> Option.map validateAll = Some true
