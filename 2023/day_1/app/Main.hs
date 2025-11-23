module Main (main) where

import Data.Char
import Data.Maybe

-- parseNumberDigit :: Char -> Maybe Char
-- parseNumberDigit c
--   | isDigit c = Just (c)
--   | otherwise = Nothing

parseNumberString :: String -> [Char]
parseNumberString [] = []
parseNumberString (x : xs) =
  if isDigit x
    then
      x : parseNumberString xs
    else parseNumberString xs

readInput :: String -> IO [String]
readInput input = do
  content <- readFile input
  return $ lines content

combineFirstAndLast :: [Char] -> Maybe Int
combineFirstAndLast [] = Nothing
combineFirstAndLast xs = do
  let frst = head xs
  let lst = last xs
  return $ read (frst : [lst])

main :: IO ()
main = do
  inputs <- readInput "input.txt"
  print inputs
  let parsedInputs = map parseNumberString inputs
  print parsedInputs
  let result = mapMaybe combineFirstAndLast parsedInputs
  print result
  print $ sum result
