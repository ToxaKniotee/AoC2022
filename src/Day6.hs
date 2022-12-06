module Day6
  ( part1
  , part2
  ) where
import Data.Set (fromList, size)

part1 :: IO Int
part1 = findStart 4 0 <$> readFile "input/day6.txt"

part2 :: IO Int
part2 = findStart 14 0 <$> readFile "input/day6.txt"
      
findStart :: (Ord a) => Int -> Int -> [a] -> Int
findStart _ _ [] = error "Reach end of stream without success"
findStart n acc xs
  | (== n) . size . fromList . take n $ xs = acc + n
  | otherwise = findStart n (acc + 1) (tail xs)