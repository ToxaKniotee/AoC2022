module Day2Tests
  ( tests
  ) where

import Day2
import Test.HUnit

tests
  = TestList
  [ TestCase $ part1 >>= assertEqual "Part 1" 11449
  , TestCase $ part2 >>= assertEqual "Part 2" 13187
  ]