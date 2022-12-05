import Day2
import Day3
import Day4
import Test.HUnit

main = runTestTTAndExit $ TestList 
  [ TestLabel "Day 2" $ TestList
    [ TestCase $ Day2.part1 >>= assertEqual "Part 1" 11449
    , TestCase $ Day2.part2 >>= assertEqual "Part 2" 13187
    ]
  , TestLabel "Day 3" $ TestList
    [ TestCase $ Day3.sample1 >>= assertEqual "Sample 1" 157
    , TestCase $ Day3.sample2 >>= assertEqual "Sample 2" 70
    , TestCase $ Day3.part1 >>= assertEqual "Part 1" 7691
    , TestCase $ Day3.part2 >>= assertEqual "Part 2" 2508
    ]
  , TestLabel "Day 4" $ TestList
    [ TestCase $ Day4.part1 >>= (@=? 471)
    , TestCase $ Day4.part2 >>= (@=? 888)
    ]
  ]