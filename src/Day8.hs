module Day8
  ( part1
  , part2
  ) where
import Data.List (singleton, transpose)
import Control.Monad (join)

part1 :: IO Int
part1 = length . filter id . findVisibles . createMatrix <$> readFile "input/day8.txt"

part2 :: IO Integer
part2 = findBiggestVisibleTree . createMatrix <$> readFile "input/day8.txt"

findVisibles matrix = zipWith (||) (join rowMatrix) (join colMatrix)
  where
    rowMatrix = calculateVisible <$> matrix
    colMatrix = transpose . fmap calculateVisible . transpose $ matrix

findBiggestVisibleTree = maximum . mapVisibleTrees

mapVisibleTrees matrix = zipWith (*) (join rowMatrix) (join colMatrix)
  where
    rowMatrix = countVisibleTrees' <$> matrix
    colMatrix = transpose . fmap countVisibleTrees' . transpose $ matrix

countVisibleTrees' values = countVisibleTrees values <$> take (length values) [0,1..]

countVisibleTrees values index = left * right
  where
    value = values !! index
    left = countTree value . reverse . take index $ values
    right = countTree value . drop (index + 1) $ values

countTree _  [] = 0
countTree ts (a:xs)
  | a == ts   = 1
  | a < ts    = 1 + countTree ts xs
  | otherwise = 1

createMatrix = ffmap (read . singleton) . lines

calculateVisible xs = zipWith (||) (visibleL xs) (visibleR xs)

visible _ [] = []
visible currentMax (a:as)
  | currentMax < a = True  : visible a          as
  | otherwise      = False : visible currentMax as

visibleL :: [Integer] -> [Bool]
visibleL = visible (-1)

visibleR = reverse . visibleL . reverse

ffmap = fmap . fmap