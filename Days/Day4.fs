module Days.Day4

open FParsec

module Day4 =
    type Color =
        | Literal of string
        | Hex of string

    type Height =
        | Inches of int32
        | Centimeters of int32

    type FieldNode =
        | BirthYear of int32
        | IssueYear of int32
        | ExpYear of int32
        | Height of Height
        | HairColor of Color
        | EyeColor of Color
        | PassportId of string
        | CountryId of string

    let intParser prefix = skipString prefix >>. pint32 .>> spaces

    let colorHexParser = skipChar '#' >>. manyChars hex |>> Hex

    let colorLiteralParser = manyChars letter |>> Literal

    let heightInchesParser = pint32 .>> skipString "in" |>> Inches

    let centimetersParser =
        pint32 .>> skipString "cm" |>> Centimeters

    let colorParser prefix =
        skipString prefix
        >>. (colorHexParser <|> colorLiteralParser)
        .>> spaces

    let idParser prefix =
        skipString prefix >>. manyChars digit .>> spaces

    let heightParser prefix =
        skipString prefix
        >>. (centimetersParser <|> heightInchesParser)
        .>> spaces

    let parseBirthYear = (intParser "byr:") |>> BirthYear
    let parseIssueYear = (intParser "iyr:") |>> IssueYear
    let parseExpYear = (intParser "eyr:") |>> ExpYear
    let parseHeight = (heightParser "hgt:") |>> Height
    let parseHairColor = (colorParser "hcl:") |>> HairColor
    let parseEyeColor = (colorParser "ecl:") |>> EyeColor
    let parsePassportId = (idParser "pid:") |>> PassportId
    let parseCountryId = (idParser "cid:") |>> CountryId

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

    type Passport =
        { BirthYear: int32 Option
          IssueYear: int32 Option
          ExpYear: int32 Option
          Height: Height Option
          HairColor: Color Option
          EyeColor: Color Option
          PassportId: string Option
          CountryId: string Option }

        static member Blank =
            { BirthYear = None
              IssueYear = None
              ExpYear = None
              Height = None
              HairColor = None
              EyeColor = None
              PassportId = None
              CountryId = None }

        member this.Valid =
            [ this.BirthYear.IsSome
              this.IssueYear.IsSome
              this.ExpYear.IsSome
              this.Height.IsSome
              this.HairColor.IsSome
              this.EyeColor.IsSome
              this.PassportId.IsSome
              this.CountryId.IsSome ]
            |> Seq.reduce (&&)

    exception PassportParseException of string

    let fillField passport field =
        match field with
        | BirthYear y when passport.BirthYear.IsNone -> Some { passport with BirthYear = Some y }
        | IssueYear y when passport.IssueYear.IsNone -> Some { passport with IssueYear = Some y }
        | ExpYear y when passport.ExpYear.IsNone -> Some { passport with ExpYear = Some y }
        | Height h when passport.Height.IsNone -> Some { passport with Height = Some h }
        | HairColor c when passport.HairColor.IsNone -> Some { passport with HairColor = Some c }
        | EyeColor c when passport.EyeColor.IsNone -> Some { passport with EyeColor = Some c }
        | PassportId i when passport.PassportId.IsNone -> Some { passport with PassportId = Some i }
        | CountryId i when passport.CountryId.IsNone -> Some { passport with CountryId = Some i }
        | _ ->
            printfn "Fill Field Failed, Field: %A Passport: %A" field passport
            None

    let buildPassport fields =
        let folder state field =
            match state with
            | Some state -> fillField state field
            | None -> None

        List.fold folder (Some Passport.Blank) fields


    let validate input =
        let output = run fieldParser input
        match output with
        | Success (result, _, _) ->
            printfn "Parsing Success: %O" result
            let passport = buildPassport result
            printfn "Build Passport Success: %O" passport

            let validatedPassport =
                match passport with
                | Some passport when passport.Valid -> Some passport
                | _ -> None

            printfn "Passport Valid: %O" validatedPassport

            validatedPassport.IsSome

        | Failure (why, _, _) ->
            printfn "Failure: %O" why
            false
