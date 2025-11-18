module Main (main) where

import Data.List (transpose)

newtype Stack a = Stack [a] deriving (Show)

emptyStack :: Stack a
emptyStack = Stack []

pushStack :: a -> Stack a -> Stack a
pushStack x (Stack xs) = Stack (x : xs)

popStack :: Stack a -> Maybe (a, Stack a)
popStack (Stack []) = Nothing
popStack (Stack (x : xs)) = Just (x, Stack xs)

popNStack :: Int -> Stack a -> Maybe ([a], Stack a)
popNStack 0 s = Just ([], s)
popNStack n s = do
  (x, s') <- popStack s
  (xs, s'') <- popNStack (n - 1) s'
  return (x : xs, s'')

pushNStack :: [a] -> Stack a -> Stack a
pushNStack xs s = foldr pushStack s xs

peekStack :: Stack a -> Maybe a
peekStack (Stack []) = Nothing
peekStack (Stack (x : _)) = Just x

parseStackLine :: String -> String
parseStackLine [] = []
parseStackLine (' ' : ' ' : ' ' : ' ' : xs) = '0' : parseStackLine xs
parseStackLine ('[' : x : ']' : xs) = x : parseStackLine xs
parseStackLine (_ : xs) = parseStackLine xs

trim :: Char -> String -> String
trim _ [] = []
trim c (x : xs) = if x == c then trim c xs else x : trim c xs

--     [D]
-- [N] [C]
-- [Z] [M] [P]
--  1   2   3
parseStack :: String -> [Stack Char]
parseStack content = do
  let linesOfFile = lines content
  let parsedLines = filter (/= "") $ map parseStackLine linesOfFile
  let transposed = transpose parsedLines
  let cleaned = map (trim '0') transposed
  map Stack cleaned

newtype Move = Move (Int, Int, Int) deriving (Show)

-- move 1 from 2 to 1
parseMoveLine :: String -> Move
parseMoveLine line = do
  let wordsInLine = words line
  let n = read (wordsInLine !! 1) :: Int
  let from = read (wordsInLine !! 3) :: Int
  let to = read (wordsInLine !! 5) :: Int
  Move (n, from, to)

-- move 1 from 2 to 1
-- move 3 from 1 to 3
-- move 2 from 2 to 1
-- move 1 from 1 to 2
parseMoves :: String -> [Move]
parseMoves input = map parseMoveLine (lines input)

--     [D]
-- [N] [C]
-- [Z] [M] [P]
--  1   2   3
--
-- move 1 from 2 to 1
-- move 3 from 1 to 3
-- move 2 from 2 to 1
-- move 1 from 1 to 2
--
-- Should split the input into two parts: the stack part and the move part
parseInput :: String -> IO (String, String)
parseInput input = do
  content <- readFile input
  let (stackPart, movePart) = break (== "") (lines content)
  let stackContent = unlines stackPart
  let moveContent = unlines (drop 1 movePart)
  return (stackContent, moveContent)

applyMove :: Move -> [Stack Char] -> [Stack Char]
applyMove (Move (n, from, to)) stacks = do
  let (Just (crates, newFromStack)) = popNStack n (stacks !! (from - 1))
  let newToStack = pushNStack (reverse crates) (stacks !! (to - 1))
  let updatedStacks = take (from - 1) stacks ++ [newFromStack] ++ drop from stacks
  take (to - 1) updatedStacks ++ [newToStack] ++ drop to updatedStacks

applyMove' :: Move -> [Stack Char] -> [Stack Char]
applyMove' (Move (n, from, to)) stacks = do
  let (Just (crates, newFromStack)) = popNStack n (stacks !! (from - 1))
  let newToStack = pushNStack crates (stacks !! (to - 1))
  let updatedStacks = take (from - 1) stacks ++ [newFromStack] ++ drop from stacks
  take (to - 1) updatedStacks ++ [newToStack] ++ drop to updatedStacks

main :: IO ()
main = do
  (stackPart, movePart) <- parseInput "input.txt"
  let stacks = parseStack stackPart
  let moves = parseMoves movePart
  print stacks
  print moves
  let finalStacks = foldl (flip applyMove') stacks moves
  print finalStacks

-- print moves

-- let finalStacks = foldl (flip applyMove) stacks moves
-- print finalStacks
