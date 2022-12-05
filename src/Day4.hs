module Day4
  ( part1
  , part2
  ) where

import Data.Set ( Set, disjoint, fromList, isSubsetOf )
import Text.Parsec (parse, digit, char, many)

part1 :: IO Int
part1 = length . filter isSubset . map parseRanges . lines <$> readFile "input/day4.txt"
  where
    isSubset (a, b) = a `isSubsetOf` b || b `isSubsetOf` a

part2 :: IO Int
part2 = length . filter hasOverlap . map parseRanges . lines <$> readFile "input/day4.txt"
  where
    hasOverlap (a, b) = not $ disjoint a b

parseRanges :: String -> (Set Int, Set Int)
parseRanges s = case parse rangesParser "" s of
  Left e -> error $ "Error parsing line " ++ show e
  Right r -> r
  where
    rangeParser = do
      a <- read <$> many digit <* char '-'
      b <- read <$> many digit
      return $ fromList [a .. b]
    rangesParser = do
      left <- rangeParser <* char ','
      right <- rangeParser
      return (left, right)
