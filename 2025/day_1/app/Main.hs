module Main (main) where

data Operation = R Int | L Int deriving (Show)

incCount :: Int -> Int -> Int
incCount s c = if s == 0 then c + 1 else c

applyOperation :: Operation -> Int -> Int -> (Int, Int)
applyOperation (R n) s c =
  let res = (s + n) `mod` 100
   in (res, incCount res c)
applyOperation (L n) s c =
  let res = (s - n + 100) `mod` 100
   in (res, incCount res c)

applyOperation' :: Operation -> Int -> Int -> (Int, Int)
applyOperation' (R n) s c =
  let res = (s + n) `mod` 100
      r = (s + n) `div` 100
   in (res, r + c)
applyOperation' (L n) s c =
  let res = (s - n + 100) `mod` 100
      r = (n - s + 99) `div` 100
   in (res, r + c)

parseLine :: String -> Operation
parseLine line =
  if head line == 'R'
    then R (read (tail line) :: Int)
    else L (read (tail line) :: Int)

readInput :: String -> IO [Operation]
readInput input = do
  contents <- readFile input
  let linesOfFile = lines contents
  return $ map parseLine linesOfFile

main :: IO ()
main = do
  operations <- readInput "input.txt"
  let finalState = foldl (\(s, c) op -> applyOperation op s c) (50, 0) operations
  print finalState
  let finalState' = foldl (\(s, c) op -> applyOperation' op s c) (50, 0) operations
  print finalState'
