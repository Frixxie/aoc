module Main where

import Data.List.Split (splitOn)

lineIntoList :: String -> [Int]
lineIntoList s = map read $ splitOn " " s

isLegal :: (Int -> Int -> Bool) -> [Int] -> Bool
isLegal _ [] = True
isLegal _ [_] = True
isLegal f (x : y : xs) = f x y && isLegal f (y : xs)

isIncreasing :: (Ord a) => a -> a -> Bool
isIncreasing a b = a < b

isDecreasing :: (Ord a) => a -> a -> Bool
isDecreasing a b = a > b

isLegalDiff :: (Ord a, Num a) => a -> a -> Bool
isLegalDiff a b = abs (a - b) < 4 && abs (a - b) > 0

-- I got help
dropping1 :: [Int] -> [[Int]]
dropping1 [] = [[]]
dropping1 (x : xs) = xs : map (x :) (dropping1 xs)

isReportLegal :: [Int] -> Bool
isReportLegal xs =
  let isInc = isLegal isIncreasing xs
      isDec = isLegal isDecreasing xs
      isDiff = isLegal isLegalDiff xs
   in (isInc || isDec) && isDiff

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let reports = map lineIntoList $ lines contents

  let legalReports = filter isReportLegal reports
  print $ length legalReports

  let legalReports' = filter (any isReportLegal . dropping1) reports
  print $ length legalReports'
