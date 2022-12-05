{-# LANGUAGE OverloadedStrings #-}

module Day5
  ( part1
  , part2
  ) where

import Data.Map.Strict as M (Map, adjust, elems, fromList, (!))
import Data.Maybe (catMaybes)
import Data.Text as T (Text, splitOn, lines)
import Data.Text.IO as T (readFile)
import Text.Parsec (char, anyChar, optional, many, parse, string, (<|>), count, digit)
import Data.List (foldl', transpose)

part1 :: IO [Char]
part1 = topCrates . moveAll . parseInput <$> T.readFile "input/day5.txt"
  where
    moveAll (m, i) = foldl' move m i
    move m (times, from, to) = iterate (moveOne from to) m !! times
    moveOne from to m = removeItem . addItem $ m
      where
        item = head $ m ! from
        removeItem = adjust tail from
        addItem = adjust (item:) to

part2 :: IO [Char]
part2 = topCrates . moveAll . parseInput <$> T.readFile "input/day5.txt"
  where
    moveAll (state, moves) = foldl' move state moves
    move m (crates, from, to) = removeCrates . addCrates $ m
      where
        crates' = take crates $ m ! from
        removeCrates = adjust (drop crates) from
        addCrates = adjust (crates' ++) to

topCrates :: Map k [b] -> [b]
topCrates = map head. elems

parseInput :: Text -> (Map Int [Char], [(Int, Int, Int)])
parseInput i = case splitOn "\n\n" i of
  [arrangement, instructions] -> (parseArrangements arrangement, parseInstructions instructions)
  _  -> error $ "Invalid number of instructions on " ++ show i
  where
    parseArrangements = fromList . zip [1..] . map catMaybes . transpose . map (liftParser lineParser) . init . T.lines
    parseInstructions = map (liftParser instructionParser) . T.lines
    liftParser p input = case parse p "" input of
      Left e -> error $ "Error durng parsing: " ++ show e
      Right a -> a
    lineParser = many charParser
    charParser = emptySpace <|> singleChar
    emptySpace = count 3 (char ' ') >> optional (char ' ') >> return Nothing
    singleChar = (Just <$>) $ char '[' >> (anyChar <* char ']' <* optional (char ' '))
    instructionParser = do
      total <- string "move " >> number
      from <- string " from " >> number
      to <- string " to " >> number
      return (total, from, to)
    number = read <$> many digit

