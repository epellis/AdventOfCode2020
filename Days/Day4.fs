module Days.Day4

open FParsec

module Day4 =
    type FieldNode =
        | BirthYear of string
        | IssueYear of string
        | ExpYear of string
        | Height of string
        | HairColor of string
        | EyeColor of string
        | PassportId of string
        | CountryId of string

    let stringParser prefix =
        skipString prefix
        >>. manyCharsTill anyChar (spaces1 <|> eof)

    let parseBirthYear = (stringParser "byr:") |>> BirthYear
    let parseIssueYear = (stringParser "iyr:") |>> IssueYear
    let parseExpYear = (stringParser "eyr:") |>> ExpYear
    let parseHeight = (stringParser "hgt:") |>> Height
    let parseHairColor = (stringParser "hcl:") |>> HairColor
    let parseEyeColor = (stringParser "ecl:") |>> EyeColor
    let parsePassportId = (stringParser "pid:") |>> PassportId
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

    type Passport =
        { BirthYear: string Option
          IssueYear: string Option
          ExpYear: string Option
          Height: string Option
          HairColor: string Option
          EyeColor: string Option
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
              this.CountryId.IsSome || this.CountryId.IsNone ]
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
            printfn "Parsing: %A" input
            printfn "Parsing Success: %A" result
            let passport = buildPassport result
            printfn "Build Passport Success: %O" passport

            let validatedPassport =
                match passport with
                | Some passport when passport.Valid -> Some passport
                | _ -> None

            printfn "Passport Valid: %O" validatedPassport

            validatedPassport.IsSome

        | Failure (why, _, _) ->
            printfn "Invalid Passport: %A" why
            raise (PassportParseException why)
