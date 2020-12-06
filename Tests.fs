module Tests

open Expecto
open Common
open Days.Day1
open Days.Day2
open Days.Day3
open Days.Day4


[<Tests>]
let tests =
    testList
        "samples"
        [ testCase "Day 1"
          <| fun _ ->
              Expect.equal (Day1.twoSum [ 1721; 979; 366; 299; 675; 1456 ] 2020) 514579 "Two sum not computed"
              Expect.equal (Day1.threeSum [ 1721; 979; 366; 299; 675; 1456 ] 2020) 241861950 "Three sum not computed"

          testCase "Day 2 Parse"
          <| fun _ ->
              Expect.equal
                  (Day2.parse "1-3 a: abcde")
                  ({ MinCount = 1
                     MaxCount = 3
                     Letter = 'a' },
                   "abcde")
                  ""
              Expect.equal
                  (Day2.parse "11-33 a: a")
                  ({ MinCount = 11
                     MaxCount = 33
                     Letter = 'a' },
                   "a")
                  ""

          testCase "Day 2"
          <| fun _ ->
              Expect.isTrue
                  (Day2.validatePartOne
                      { MinCount = 1
                        MaxCount = 3
                        Letter = 'a' }
                       "abcde")
                  ""
              Expect.isFalse
                  (Day2.validatePartOne
                      { MinCount = 1
                        MaxCount = 3
                        Letter = 'b' }
                       "cdefg")
                  ""
              Expect.isTrue
                  (Day2.validatePartOne
                      { MinCount = 1
                        MaxCount = 9
                        Letter = 'c' }
                       "ccccccccc")
                  ""

              Expect.isTrue
                  (Day2.validatePartTwo
                      { MinCount = 1
                        MaxCount = 3
                        Letter = 'a' }
                       "abcde")
                  ""
              Expect.isFalse
                  (Day2.validatePartTwo
                      { MinCount = 1
                        MaxCount = 3
                        Letter = 'b' }
                       "cdefg")
                  ""
              Expect.isFalse
                  (Day2.validatePartTwo
                      { MinCount = 1
                        MaxCount = 9
                        Letter = 'c' }
                       "ccccccccc")
                  ""

          testCase "Day 3"
          <| fun _ ->
              Expect.equal (Day3.parseLine ".#.") [| Day3.Snow; Day3.Tree; Day3.Snow |] ""

              let day3Landscape =
                  Common.fileToStringList "/Users/e/Documents/AdventOfCode/inputs/Day3Test.txt"
                  |> Day3.parseInput

              Expect.equal (Day3.countTrees day3Landscape { Day3.X = 0; Day3.Y = 0 } { Day3.X = 3; Day3.Y = 1 }) 7 ""

          testCase "Day 4"
          <| fun _ ->
              Expect.isTrue
                  (Day4.validate "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
                  ""

              Expect.isFalse (Day4.validate "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929") ""
              Expect.isTrue (Day4.validate "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm") ""
              Expect.isFalse (Day4.validate "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in") ""

              let passportInputs =
                  Common.fileToNewlineSeparatedList "/Users/e/Documents/AdventOfCode/inputs/Day4Test.txt"

              let validations =
                  passportInputs
                  |> Seq.map Day4.validate
                  |> Seq.filter id
                  |> Seq.length

              Expect.equal validations 2 "" ]
