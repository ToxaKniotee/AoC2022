module Day2
  ( part1
  , part2
  ) where
import Data.Maybe (fromJust)
import Data.Tuple (swap)

data Hand
  = Rock
  | Paper
  | Scissor
  deriving (Eq)

values :: [Hand]
values = [Rock, Paper, Scissor]

winHands :: [(Hand, Hand)]
winHands = zip values $ tail $ cycle values

drawHands :: [(Hand, Hand)]
drawHands = zip values values

loseHands :: [(Hand, Hand)]
loseHands = swap <$> winHands

part1 :: IO Int
part1 = sum . (snd . score . parseInput1 <$>) . lines <$> readFile "input/day2.txt"

part2 :: IO Int
part2 = sum . (snd . score . parseInput2 <$>) . lines <$> readFile "input/day2.txt"

parseInput1 :: String -> (Hand, Hand)
parseInput1 s = case s of
  [a,' ',b] -> (parseLeft a, parseRight)
    where
      parseRight = fromJust . lookup b $ zip "XYZ" values
  e -> error $ "Unknown pattern " ++ e

parseInput2 :: String -> (Hand, Hand)
parseInput2 s = case s of
  [a, ' ', b] -> (left, right)
    where
      left = parseLeft a
      outcome = fromJust . lookup b $ zip "XYZ" [loseHands, drawHands, winHands]
      right = fromJust . lookup left $ outcome
  e -> error $ "UNknown pattern for line " ++ e


parseLeft :: Char -> Hand
parseLeft a = fromJust . lookup a $ zip "ABC" values

score :: (Hand, Hand) -> (Int, Int)
score hand@(a, b) = (a' + handScore a, b' + handScore b)
  where
    (a', b') = winScore hand

handScore :: Num a => Hand -> a
handScore hand = fromJust . lookup hand $ zip values [1, 2, 3]

winScore :: (Num a, Num b) => (Hand, Hand) -> (a, b)
winScore hand
  | hand `elem` loseHands = (6, 0)
  | hand `elem` drawHands = (3, 3)
  | otherwise = (0, 6)