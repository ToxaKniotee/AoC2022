module Day2
  ( part1
  , part2
  ) where
import Data.Maybe (fromJust)

data Hand
  = Rock
  | Paper
  | Scissor
  deriving (Eq)

values :: [Hand]
values = [Rock, Paper, Scissor]

winHands :: [(Hand, Hand)]
winHands = zip values (tail values ++ [head values])

drawHands :: [(Hand, Hand)]
drawHands = zip values values

loseHands :: [(Hand, Hand)]
loseHands = zip (tail values ++ [head values]) values

part1 :: IO Int
part1 = sum . (snd . score . parseInput1 <$>) . lines <$> readFile "input/day2.txt"

part2 :: IO Int
part2 = sum . (snd . score . parseInput2 <$>) . lines <$> readFile "input/day2.txt"

parseInput1 :: String -> (Hand, Hand)
parseInput1 s = case s of
  [a,' ',b] -> (parseLeft a, parseRight)
    where
      parseRight = case b of
        'X' -> Rock
        'Y' -> Paper
        'Z' -> Scissor
        _ -> error $ "Unknown case :" ++ [a]
  e -> error $ "Unknown pattern " ++ e

parseInput2 :: String -> (Hand, Hand)
parseInput2 s = case s of
  [a, ' ', b] -> (left, right)
    where
      left = parseLeft a
      outcome = case b of
        'X' -> loseHands
        'Y' -> drawHands
        'Z' -> winHands
        _ -> error $ "Unknown case " ++ [b]
      right = fromJust . lookup left $ outcome
  e -> error $ "UNknown pattern for line " ++ e


parseLeft :: Char -> Hand
parseLeft a = case a of
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissor
  _ -> error $ "Unknown case :" ++ [a]

score :: (Hand, Hand) -> (Int, Int)
score hand@(a, b) = (a' + handScore a, b' + handScore b)
  where
    (a', b') = winScore hand

handScore :: Num a => Hand -> a
handScore hand = case hand of
  Rock -> 1
  Paper -> 2
  Scissor -> 3

winScore :: (Num a, Num b) => (Hand, Hand) -> (a, b)
winScore hand
  | hand `elem` loseHands = (6, 0)
  | hand `elem` drawHands = (3, 3)
  | otherwise = (0, 6)