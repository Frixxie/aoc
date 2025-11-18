module Main (main) where

-- Checks if N first chars is distinct
isDistinct :: String -> Int -> Bool
isDistinct str n = length (take n str) == length (distinctChars (take n str))
  where
    distinctChars [] = []
    distinctChars (x : xs)
      | x `elem` xs = distinctChars xs
      | otherwise = x : distinctChars xs

findFirstMarkerAfter :: String -> Int -> Int
findFirstMarkerAfter [] i = i
findFirstMarkerAfter str i
  | isDistinct str 14 = i + 14
  | otherwise = findFirstMarkerAfter (tail str) (i + 1)

readInput :: String -> IO [String]
readInput path = do
  contents <- readFile path
  return (lines contents)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let result = findFirstMarkerAfter input 0
  print result
