module Main (main) where

import Data.Foldable

data Operation = R Int | L Int deriving (Show)

applyOperation :: Operation -> Int -> Int -> IO (Int, Int)
applyOperation (R n) s c = do
  let res = (s + n) `mod` 100
  print res
  return (res, if res == 0 then c + 1 else c)
applyOperation (L n) s c = do
  let res = (s - n + 100) `mod` 100
  print res
  return (res, if res == 0 then c + 1 else c)

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
  print operations
  finalState <- foldlM (\(s, c) op -> applyOperation op s c) (50, 0) operations
  print finalState
