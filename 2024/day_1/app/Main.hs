module Main where

import Data.Either
import Data.List
import Data.Map (Map, adjust, empty, insert, lookup, member)
import qualified Data.Maybe
import Text.Parsec

parseDigits :: Parsec String () ([Char], [Char])
parseDigits = do
  frst <- many digit
  spaces
  lst <- many digit
  return (frst, lst)

findAllDigits :: String -> Either ParseError (Int, Int)
findAllDigits line = do
  (frst, lst) <- parse parseDigits "" line
  return (read frst, read lst)

diff :: (Int, Int) -> Int
diff (a, b) = abs $ a - b

numTimesInOtherList :: Int -> [Int] -> Int
numTimesInOtherList item l = length $ filter (item ==) l

numTimesInOtherList' :: Int -> Map Int Int -> Int
numTimesInOtherList' item m = Data.Maybe.fromMaybe 0 $ Data.Map.lookup item m

adjustOrInsert :: Int -> Map Int Int -> Map Int Int
adjustOrInsert item m =
  if member item m
    then
      adjust (+ 1) item m
    else
      Data.Map.insert item 1 m

main :: IO ()
main = do
  contents <- readFile "input.txt"

  let digits = rights $ map findAllDigits $ lines contents

  let (frst, lst) = unzip digits

  -- Part I
  let frst_sorted = sort frst
  let lst_sorted = sort lst
  let result1 = foldr (\item acc -> acc + diff item) 0 $ zip frst_sorted lst_sorted
  print result1

  -- Part II
  -- Naive try
  let result2 = foldr (\item acc -> acc + item * numTimesInOtherList item lst) 0 frst
  print result2

  -- Use table instead
  let lookuptbl = foldr adjustOrInsert empty lst
  let result3 = foldr (\item acc -> acc + item * numTimesInOtherList' item lookuptbl) 0 frst
  print result3
