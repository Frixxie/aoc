module Main where

import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Text.Parsec

data Rule = Rule Int Int deriving (Show)

data PrinterUpdate = PrinterUpdate [Int] deriving (Show)

parseRule :: Parsec String () Rule
parseRule = do
  left <- many digit
  _ <- char '|'
  right <- many digit
  return $ Rule (read left) (read right)

parsePrinterUpdates :: Parsec String () PrinterUpdate
parsePrinterUpdates = do
  nums <- sepBy (many digit) (char ',')
  return $ PrinterUpdate $ map read nums

findAllRules :: [String] -> [Rule]
findAllRules lins = rights $ map (parse parseRule "") lins

findPrinterUpdates :: [String] -> [PrinterUpdate]
findPrinterUpdates lins = rights $ map (parse parsePrinterUpdates "") lins

splitIntoParts :: [String] -> ([String], [String])
splitIntoParts [] = ([], [])
splitIntoParts (x : xs) = if x == "" then ([], xs) else (x : a, b)
  where
    (a, b) = splitIntoParts xs

checkRule :: [Int] -> [Int] -> Bool
checkRule [] _ = True
checkRule (x : xs) rules = (x `notElem` rules) && checkRule xs rules

isUpdateValid :: [Int] -> Map.Map Int [Int] -> Bool
isUpdateValid [] _ = True
isUpdateValid (x : xs) rules = if isNothing (Map.lookup x rules) then isUpdateValid xs rules else checkRule xs (rules Map.! x) && isUpdateValid xs rules

ruleIntoMap :: [Rule] -> Map.Map Int [Int] -> Map.Map Int [Int]
ruleIntoMap [] m = m
ruleIntoMap (Rule a b : xs) m = ruleIntoMap xs $ if isNothing (Map.lookup a m) then Map.insert a [b] m else Map.insert a (b : (m Map.! a)) m

findMiddleUpdates :: [Int] -> Maybe Int
findMiddleUpdates [] = Nothing
findMiddleUpdates [x] = Just x
findMiddleUpdates xs = Just $ xs !! (length xs `div` 2)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  (rulesInput, printerUpdatesInput) <- return $ splitIntoParts $ lines contents
  let rules = ruleIntoMap (findAllRules rulesInput) Map.empty
  print rules
  print $ findPrinterUpdates printerUpdatesInput

  let result = filter (\update -> isUpdateValid (reverse update) rules) (map (\(PrinterUpdate x) -> x) (findPrinterUpdates printerUpdatesInput))
  print result

  let middleUpdates = mapMaybe findMiddleUpdates result
  print $ sum middleUpdates
