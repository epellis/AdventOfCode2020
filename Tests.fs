module Tests

open Expecto
open Days.Day1

[<Tests>]
let tests =
  testList "samples" [
    testCase "Day 1.1" <| fun _ ->
      Expect.equal (Day1.twoSum [1721; 979; 366; 299; 675; 1456] 2020) 514579 "Two sum not computed"

    testCase "Day 1.2" <| fun _ ->
      Expect.equal (Day1.threeSum [1721; 979; 366; 299; 675; 1456] 2020) 241861950 "Three sum not computed"
  ]
