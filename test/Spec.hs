import Day2Tests as Day2
import Test.HUnit

main = runTestTTAndExit $ TestList 
  [ TestLabel "Day 2" Day2.tests
  ]