module Main where

import qualified Data.Bifunctor
import qualified Data.Map

data Grid = Tile Int Int Char deriving (Show)

contentToGrid :: String -> Int -> Int -> [Grid]
contentToGrid [] _ _ = []
contentToGrid (x : xs) row col =
  if x == '\n'
    then contentToGrid xs (row + 1) 0
    else Tile row col x : contentToGrid xs row (col + 1)

gridToMap :: [Grid] -> Data.Map.Map (Int, Int) Char -> Data.Map.Map (Int, Int) Char
gridToMap [] m = m
gridToMap (Tile row col c : xs) m = gridToMap xs (Data.Map.insert (row, col) c m)

findXmasRow :: Int -> Int -> Int -> Data.Map.Map (Int, Int) Char -> [Maybe Char]
findXmasRow _ _ 0 _ = []
findXmasRow row col lim m =
  case Data.Map.lookup (row, col) m of
    Just c -> Just c : findXmasRow row (col + 1) (lim - 1) m
    Nothing -> []

findXmasCol :: Int -> Int -> Int -> Data.Map.Map (Int, Int) Char -> [Maybe Char]
findXmasCol _ _ 0 _ = []
findXmasCol row col lim m =
  case Data.Map.lookup (row, col) m of
    Just c -> Just c : findXmasCol (row + 1) col (lim - 1) m
    Nothing -> []

findXmasRightDiag :: Int -> Int -> Int -> Data.Map.Map (Int, Int) Char -> [Maybe Char]
findXmasRightDiag _ _ 0 _ = []
findXmasRightDiag row col lim m =
  case Data.Map.lookup (row, col) m of
    Just c -> Just c : findXmasRightDiag (row + 1) (col + 1) (lim - 1) m
    Nothing -> []

findXmasLeftDiag :: Int -> Int -> Int -> Data.Map.Map (Int, Int) Char -> [Maybe Char]
findXmasLeftDiag _ _ 0 _ = []
findXmasLeftDiag row col lim m =
  case Data.Map.lookup (row, col) m of
    Just c -> Just c : findXmasLeftDiag (row + 1) (col - 1) (lim - 1) m
    Nothing -> []

findAllRightDiagXmas :: Int -> [Grid] -> Data.Map.Map (Int, Int) Char -> [([Maybe Char], (Int, Int))]
findAllRightDiagXmas _ [] _ = []
findAllRightDiagXmas lim (Tile row col _ : xs) m =
  (findXmasRightDiag row col lim m, (row, col))
    : findAllRightDiagXmas lim xs m

findAllLeftDiagXmas :: Int -> [Grid] -> Data.Map.Map (Int, Int) Char -> [([Maybe Char], (Int, Int))]
findAllLeftDiagXmas _ [] _ = []
findAllLeftDiagXmas lim (Tile row col _ : xs) m =
  (findXmasLeftDiag row col lim m, (row, col))
    : findAllLeftDiagXmas lim xs m

findAllXmas :: Int -> [Grid] -> Data.Map.Map (Int, Int) Char -> [[Maybe Char]]
findAllXmas _ [] _ = []
findAllXmas lim (Tile row col _ : xs) m =
  findXmasRow row col lim m
    : findXmasCol row col lim m
    : findXmasRightDiag row col lim m
    : findXmasLeftDiag row col lim m
    : findAllXmas lim xs m

convertToString :: [Maybe Char] -> String
convertToString [] = ""
convertToString (Just c : xs) = c : convertToString xs
convertToString (Nothing : xs) = convertToString xs

checkIfCross :: (String, (Int, Int)) -> [(String, (Int, Int))] -> Bool
checkIfCross _ [] = False
checkIfCross (s, (x, y)) ((_, (a, b)) : xs) = ((x + 1) == (a + 1) && (y + 1) == (b - 1)) || checkIfCross (s, (x, y)) xs

main :: IO ()
main = do
  content <- readFile "input.txt"
  let grid = contentToGrid content 0 0
  let m = gridToMap grid Data.Map.empty

  let allXmas = map convertToString $ filter (\a -> length a > 3) $ findAllXmas 4 grid m
  let result1 = filter (\a -> a == "XMAS" || reverse a == "XMAS") allXmas

  print $ length result1

  let allRight = filter (\(a, _) -> a == "MAS" || reverse a == "MAS") $ map (Data.Bifunctor.first convertToString) $ filter (\(a, _) -> length a > 2) $ findAllRightDiagXmas 3 grid m
  let allLeft = filter (\(a, _) -> a == "MAS" || reverse a == "MAS") $ map (Data.Bifunctor.first convertToString) $ filter (\(a, _) -> length a > 2) $ findAllLeftDiagXmas 3 grid m

  let result2 = filter (`checkIfCross` allLeft) allRight
  print $ length result2
