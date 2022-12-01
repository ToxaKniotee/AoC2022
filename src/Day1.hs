module Day1
  ( part1
  , part2
  ) where

import Data.List ( sort )

readInput :: IO [Integer]
readInput = (calculateCalories <$>) . split . lines <$> readFile "input/day1.txt"

part1 :: IO Integer
part1 = maximum <$> readInput

part2 :: IO Integer
part2 = sum . take 3 . reverse . sort <$> readInput

calculateCalories  :: [String] -> Integer
calculateCalories = sum . fmap read

split :: [String] -> [[String]]
split [] = []
split a = left : (split . safeTail) right
  where 
    (left, right) = span (/= "") a

safeTail :: [a] -> [a]
safeTail [] = []
safeTail a = tail a