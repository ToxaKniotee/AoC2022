module Day7
  ( part1
  , part2
  ) where
import Text.Parsec (Parsec, many, digit, newline, string, parse, try, choice, parserTraced)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (manyTill)
import Text.ParserCombinators.Parsec ((<|>))
import Data.Tree (Tree(Node), flatten)
import Data.List (sort)
import Debug.Trace (trace)

data Command
  = Cd String
  | Ls Int
  deriving (Show)

type Folder = Tree (String, Int)

-- part1 :: IO Int
part1 :: IO Int
part1 = sum . filter (<= 100000) . tail . parseInput <$> readFile "input/day7.txt"

part2 :: IO Int
part2 = f . parseInput <$> readFile "input/day7.txt"
  where
    f [] = error "Emtpy Items"
    f (x:xs) = head . dropWhile (isNotEnough x) . reverse . sort $ xs
    isNotEnough currentUse folderSize = currentUse - folderSize + 30000000 > 70000000

calculateDirs :: [Command] -> [Folder] -> Folder
-- No Commands left to process, single folder (root)
calculateDirs [] [folder] = folder
-- No commands left, folders still left in the stack
calculateDirs [] folders = calculateDirs [Cd ".."] folders 
-- Commands available
calculateDirs (command:commands) dirStack = case command of
  Cd ".." -> calculateDirs commands .foldOne $ dirStack
  Cd newDir -> calculateDirs commands . (createTree newDir:) $ dirStack
  -- Ls bytes -> calculateDirs commands $ mapHead (mapValue (mapSize (+ bytes))) dirStack
  Ls bytes -> calculateDirs commands . addSize' bytes $ dirStack
  where
    addSize' b' (Node (n, b) fs' : ns) = Node (n, b + b') fs' : ns
    addSize' _ _ = error "Attempt to modify non existing folder"
    createTree n = Node (n, 0) []
    foldOne ( child@(Node (_, s') _) : Node (pn, ps) pc :stack ) = Node (pn, ps + s') (child:pc) : stack
    foldOne _ = error "Invalid configuration for fold "

parseInput :: String -> [Int]
parseInput = map snd . flatten . (`calculateDirs` []) . liftParser inputParser

liftParser :: Parsec String () a -> String -> a
liftParser p i = case parse p "" i of
  Left e -> error $ "Error during parsing " ++ show e
  Right a -> a

inputParser :: Parsec String () [Command]
inputParser = many commands
  where
    commands = try cd <|> try ls
    cd = parserTraced "cd" $ string "$ cd " >> (Cd <$> folderName)
    folderName = parserTraced "folderName" $ manyTill anyChar newline
    ls = parserTraced "ls" $ string "$ ls" >> newline >> (Ls <$> (sum <$> many files))
    files = try file <|> try dir
    file = parserTraced "fileSize" $ number <* manyTill anyChar newline
    dir = parserTraced "dir" $ string "dir " >> manyTill anyChar newline >> pure 0
    number = (p "Number" <$>) $ parserTraced "numbr" $ read <$> many digit
    p t a = trace (t <> show a) a