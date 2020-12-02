module Tests

open Expecto
open Days.Day1
open Days.Day2

[<Tests>]
let tests =
  testList "samples" [
    testCase "Day 1" <| fun _ ->
      Expect.equal (Day1.twoSum [1721; 979; 366; 299; 675; 1456] 2020) 514579 "Two sum not computed"
      Expect.equal (Day1.threeSum [1721; 979; 366; 299; 675; 1456] 2020) 241861950 "Three sum not computed"

    testCase "Day 2 Parse" <| fun _ ->
      Expect.equal (Day2.parse "1-3 a: abcde") ({ MinCount = 1; MaxCount = 3; Letter = 'a'}, "abcde") ""
      Expect.equal (Day2.parse "11-33 a: a") ({ MinCount = 11; MaxCount = 33; Letter = 'a'}, "a") ""

    testCase "Day 2.1" <| fun _ ->
      Expect.isTrue (Day2.validatePartOne { MinCount = 1; MaxCount = 3; Letter = 'a'} "abcde") ""
      Expect.isFalse (Day2.validatePartOne { MinCount = 1; MaxCount = 3; Letter = 'b'} "cdefg") ""
      Expect.isTrue (Day2.validatePartOne { MinCount = 1; MaxCount = 9; Letter = 'c'} "ccccccccc") ""
  ]
