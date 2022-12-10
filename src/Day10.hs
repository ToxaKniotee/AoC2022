module Day10
  ( part1
  , part2
  ) where
import Control.Monad ((<=<))

data Instruction
  = Noop
  | AddValue Int

part1 = sum . takeValues . compute 1 . map parseLine . lines <$> readFile "input/day10.txt"
  where
    takeValues v = f <$> [20, 60, 100, 140, 180, 220]
      where f i = i * (v !! (i - 1))

part2 = printMatrix . compute 1 . map parseLine . lines <=< readFile $ "input/day10.txt"

printMatrix = printToConsole . drawEachLine . separateLines
  where
    separateLines = group 40
    drawEachLine = map drawLine
    printToConsole = mapM_ putStrLn

drawLine = zipWith drawChar [0,1..]

drawChar currentChar spriteOffset = sprite !! currentChar
  where
    sprite = replicate (spriteOffset -1) '.' ++ "###" ++ replicate 60 '.'

compute v [] = [v]
compute v (Noop:xs)          = v : compute v xs
compute v (AddValue v' : xs) = v : v : compute (v + v') xs

parseLine "noop" = Noop
parseLine xs = AddValue (read . drop 5 $ xs)

group _ [] = []
group n values = chunk : group n reminder
  where (chunk, reminder) = splitAt n values