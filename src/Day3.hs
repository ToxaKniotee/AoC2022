module Day3
  ( sample1
  , sample2
  , part1
  , part2
  ) where

import Data.List (sort)
import Data.Maybe (fromJust)

sample1 :: IO Int
sample1 = part1' <$> readFile "sample/day3.txt"

sample2 :: IO Int
sample2 = part2' <$> readFile "sample/day3.txt"

part1 :: IO Int
part1 = part1' <$> readFile "input/day3.txt"

part2 :: IO Int
part2 = part2' <$> readFile "input/day3.txt"

part1' :: String -> Int
part1' = sum . (priority . common . halfs <$>) . lines

part2' :: String -> Int
part2' = sum . (priority . findCommon <$>) . (sort <$$>) . chunks 3 . lines

halfs :: [a] -> ([a], [a])
halfs xs = splitAt (length xs `div` 2) xs

common :: Ord a => ([a], [a]) -> a
common (as, bs) = findCommon [as', bs']
  where
    as' = sort as
    bs' = sort bs

findCommon :: (Ord c) => [[c]] -> c
findCommon xss
  | all (== minHead) (heads xss) = minHead
  | otherwise = findCommon $ dropWhile (== minHead) <$> xss
  where
    heads = map head
    minHead = minimum . heads $ xss

priority :: Char -> Int
priority a = fromJust $ lookup a $ zip chars [1,2..]
  where
    chars = ['a'..'z'] ++ ['A' .. 'Z']

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = left : chunks n right
  where
    (left, right) = splitAt 3 xs

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
a <$$> b = (a <$>) <$> b