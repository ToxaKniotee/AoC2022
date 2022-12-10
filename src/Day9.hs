module Day9
  ( part1
  , part2
  ) where

import Math.Geometry.Grid
import Math.Geometry.Grid.Octagonal
import Math.Geometry.Grid.Square
import Data.List (find, scanl')
import Data.Set (insert)
import qualified Data.Set as S (singleton, size)
import Data.Maybe (fromJust)

part1 = S.size . last . calculateSteps . parseInput <$> readFile "input/day9.txt"
  where
    calculateSteps steps = calculateStep (0, 0) (replicate 1 (0,0)) steps $ replicate 1 $ S.singleton (0,0)

part2 = S.size . last . calculateSteps . parseInput <$> readFile "input/day9.txt"
  where
    calculateSteps steps = calculateStep (0,0) (replicate 9 (0,0)) steps (replicate 9 (S.singleton (0,0)))

parseInput s = lines s >>= getFunction

getFunction (c : ' ' : ns)
  | c == 'L' = replicate (read ns) $ \(x, y) -> (x - 1, y)
  | c == 'R' = replicate (read ns) $ \(x, y) -> (x + 1, y)
  | c == 'D' = replicate (read ns) $ \(x, y) -> (x, y - 1)
  | c == 'U' = replicate (read ns) $ \(x, y) -> (x, y + 1)
  | otherwise = error $ "Unknown character " ++ show c
getFunction l = error $ "Unknown line patter " ++ l

calculateStep headPoint tailPoints (step: steps) sets = calculateStep newHead newTails steps newSets
  where
    newHead = step headPoint
    newTails = tail $ scanl' moveTail newHead tailPoints
    newSets = zipWith insert newTails sets
calculateStep _ _ [] sets = sets

moveTail target@(tx, ty) current@(cx, cy) = newTail
  where
    distanceToHead = distance UnboundedOctGrid current target
    isSameAxis = tx == cx || ty == cy
    allMoves = adjacentTilesToward UnboundedOctGrid current target
    linearMoves = adjacentTilesToward UnboundedSquareGrid current target
    diagonalMove = find (\(px, py) -> px /= cx && py /= cy) allMoves
    getNewTail
      | isSameAxis = head linearMoves
      | otherwise = fromJust diagonalMove
    newTail
      | distanceToHead >= 2 = getNewTail
      | otherwise = current
