import Day2
import Test.HUnit

main = runTestTTAndExit $ TestList 
  [ TestLabel "Day 2" $ TestList
    [ TestCase $ Day2.part1 >>= assertEqual "Part 1" 11449
    , TestCase $ Day2.part2 >>= assertEqual "Part 2" 13187
    ]
  ]