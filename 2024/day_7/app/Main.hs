module Main where

import Data.List.Split (splitOn)
import Data.Maybe
import Control.Parallel.Strategies (parList, rseq, using)

data Calc = Calc Int [Int]

interprateInput :: String -> Maybe Calc
interprateInput input = do
  let parts = splitOn ":" input
  case parts of
    [ansStr, numsStr] -> do
      let answer = read ansStr
      let nums = map read $ words numsStr
      return $ Calc answer nums
    _ -> Nothing

calculateInput :: [Int] -> [Int]
calculateInput [] = []
calculateInput [x] = [x]
calculateInput (x : y : xs) = calculateInput (x + y : xs) ++ calculateInput ((x * y) : xs)

concat' :: Int -> Int -> Int
concat' a b = read $ show a ++ show b

calculateInput' :: [Int] -> [Int]
calculateInput' [] = []
calculateInput' [x] = [x]
calculateInput' (x : y : xs) =
  calculateInput' ((x + y) : xs)
    ++ calculateInput' ((x * y) : xs)
    ++ calculateInput' (x `concat'` y : xs)

isCalcValid :: Calc -> Bool
isCalcValid (Calc answer nums) = answer `elem` calculateInput nums

isCalcValid' :: Calc -> Bool
isCalcValid' (Calc answer nums) = answer `elem` calculateInput' nums

main :: IO ()
main = do
  content <- readFile "input.txt"
  let inputs = mapMaybe interprateInput (lines content)
  let bools = map isCalcValid inputs `using` parList rseq
  let results = map fst $ filter snd $ zip inputs bools
  let result = foldr (\(Calc r _) acc -> r + acc) 0 results
  print result

  let bools' = map isCalcValid' inputs `using` parList rseq
  let results' = map fst $ filter snd $ zip inputs bools'
  let result' = foldr (\(Calc r _) acc -> r + acc) 0 results'
  print result'
