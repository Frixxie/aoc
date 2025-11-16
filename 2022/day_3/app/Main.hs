module Main (main) where

import Data.Char

data Rucksack = Rucksack String String

wholeRucksack :: Rucksack -> String
wholeRucksack (Rucksack fc sc) = fc ++ sc

findDuplicateItem :: Rucksack -> Maybe Char
findDuplicateItem (Rucksack [] []) = Nothing
findDuplicateItem (Rucksack [] _) = Nothing
findDuplicateItem (Rucksack _ []) = Nothing
findDuplicateItem (Rucksack (x : xs) ys) = if x `elem` ys then Just x else findDuplicateItem (Rucksack xs ys)

findCommonItem :: String -> String -> String -> Maybe Char
findCommonItem [] _ _ = Nothing
findCommonItem (x : xs) ys zs =
  if x `elem` ys && x `elem` zs
    then Just x
    else findCommonItem xs ys zs

parseRucksack :: String -> Rucksack
parseRucksack str =
  let len = length str
      (fc, sc) = splitAt (len `div` 2) str
   in Rucksack fc sc

readRucksacks :: String -> IO [Rucksack]
readRucksacks filePath = do
  contents <- readFile filePath
  let rucksacks = map parseRucksack (lines contents)
  return rucksacks

itemToPriority :: Char -> Int
itemToPriority c
  | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
  | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = 0

findCommonItemsInGroups :: [Rucksack] -> [Maybe Char]
findCommonItemsInGroups [] = []
findCommonItemsInGroups [_] = []
findCommonItemsInGroups [_, _] = []
findCommonItemsInGroups (r1 : r2 : r3 : rs) =
  let commonItem = findCommonItem (wholeRucksack r1) (wholeRucksack r2) (wholeRucksack r3)
   in commonItem : findCommonItemsInGroups rs

main :: IO ()
main = do
  rucksacks <- readRucksacks "input.txt"
  let duplicates = map findDuplicateItem rucksacks
  let priorities = [itemToPriority c | Just c <- duplicates]
  print (priorities, sum priorities)

  let commonItems = findCommonItemsInGroups rucksacks
  let commonPriorities = [itemToPriority c | Just c <- commonItems]
  print (commonPriorities, sum commonPriorities)
