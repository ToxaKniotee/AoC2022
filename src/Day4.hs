module Day4
  ( part1
  , part2
  ) where

import Data.Set as S
import Data.List as L (filter)
import Text.Parsec (parse, digit, char, Parsec)
import Control.Applicative

part1 :: IO Int
part1 = length . L.filter isSubset . (parseInput <$>) . lines <$> readFile "input/day4.txt"
  where
    isSubset (a, b) = a `isSubsetOf` b || b `isSubsetOf` a

part2 :: IO Int
part2 = sum . fmap (intersect . parseInput) . lines <$> readFile "input/day4.txt"
  where
    intersect (a, b) = if a `disjoint` b then 0 else 1

parseInput :: String -> (Set Int, Set Int)
parseInput s = case parse parseRanges "" s of
  Left e -> error $ "Error parsing line " ++ show e
  Right r -> r

parseRanges :: Parsec String () (Set Int, Set Int)
parseRanges = do
  left <- parseRange <* char ','
  right <- parseRange
  return (left, right)

parseRange :: Parsec String () (Set Int)
parseRange = do
  a <- read <$> many digit <* char '-'
  b <- read <$> many digit
  return $ fromList [a .. b]
