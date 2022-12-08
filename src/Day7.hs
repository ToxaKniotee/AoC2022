module Day7
  ( part1
  , part2
  ) where
import Text.Parsec (Parsec, many, digit, char, newline, string, parse, try)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (manyTill)
import Text.ParserCombinators.Parsec ((<|>))
import Data.Tree (Tree(Node, rootLabel), flatten, rootLabel)
import Data.List (sort, foldl1')

data Command
  = Cd String
  | Ls [LsEntry]
  deriving (Show)

data LsEntry
  = File Int String
  | Directory String
  deriving (Show)

type Folder = Tree (String, Int)

getFileSize :: LsEntry -> Int
getFileSize (File n _) = n
getFileSize _ = 0

addSubDirectory :: Folder -> Folder -> Folder
addSubDirectory child (Node (pn, ps) sub) = Node (pn, newSize) (child:sub)
  where
    newSize = (snd . rootLabel) child + ps

addFile :: Int -> Folder -> Folder
addFile s' (Node (n, s) sub) = Node (n, s + s') sub

part1 :: IO Int
part1 = findSubDirs 100000 . (`calculateDirs` []) . parseInput <$> readFile "input/day7.txt"
  where
    findSubDirs minSize (Node _ children) = sum . filter (<= minSize) . allSubFolders $ children

part2 :: IO Int
part2 = findDirToDelete . (`calculateDirs` []) . parseInput <$> readFile "input/day7.txt"
  where
    findDirToDelete (Node (_, usedSpace) dirs) = head . dropWhile (isNotEnoughSpace usedSpace) . sort . allSubFolders $ dirs
    isNotEnoughSpace usedSpace folderSize = usedSpace - folderSize + 30000000 > 70000000

calculateDirs :: [Command] -> [Folder] -> Folder
-- No Commands left to process, single folder (root)
calculateDirs [] [folder] = folder
-- Commands available
calculateDirs (command:commands) dirStack = case command of
  Cd ".." -> calculateDirs commands updatedStack
    where
      updatedStack = newHead:folders
      (left, folders) = splitAt 2 dirStack
      newHead = foldl1' addSubDirectory left
  Cd newDir -> calculateDirs commands $ Node (newDir, 0) [] : dirStack
  Ls files -> calculateDirs commands (newParent : tail dirStack)
    where
      filesSize = sum $ getFileSize <$> files
      newParent = addFile filesSize (head dirStack)
-- No commands left, folders still left in the stack
calculateDirs [] folders = calculateDirs [Cd ".."] folders 

allSubFolders :: [Tree (a, b)] -> [b]
allSubFolders children = (snd <$>) $ children >>= flatten

parseInput :: String -> [Command]
parseInput = liftParser inputParser

liftParser :: Parsec String () a -> String -> a
liftParser p i = case parse p "" i of
  Left e -> error $ "Error during parsing " ++ show e
  Right a -> a

inputParser :: Parsec String () [Command]
inputParser = many commands
  where
    commands = try cd <|> try ls
    cd = string "$ cd " >> Cd <$> folderName
    folderName = manyTill anyChar newline
    ls = string "$ ls" >> newline >> Ls <$> many files
    files = try file <|> try dir
    file = File <$> number <*> fileName
    fileName = char ' ' >> manyTill anyChar newline
    dir = string "dir " >> Directory <$> manyTill anyChar newline
    number = read <$> many digit