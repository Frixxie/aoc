module Main (main) where
import Data.Ord (comparing, Down (..))
import Data.List

-- takes a file of cals and return a list of lists
readfile :: String -> IO [[Int]]
readfile filename = do
  contents <- readFile filename
  let contentlines = lines contents
  let groupedLines = splitOnEmptyLines contentlines
  return $ map (map read) groupedLines

splitOnEmptyLines :: [String] -> [[String]]
splitOnEmptyLines [] = []
splitOnEmptyLines xs =
  let (firstGroup, rest) = break null xs
   in firstGroup : case rest of
        [] -> []
        (_ : ys) -> splitOnEmptyLines ys

sumEachList :: [[Int]] -> [Int]
sumEachList = map sum

findAndSumTopThree :: [[Int]] -> Int
findAndSumTopThree lists =
  let sums = sumEachList lists
      topThree = take 3 $ sortBy (comparing Data.Ord.Down) sums
   in sum topThree

main :: IO ()
main = do
  lists <- readfile "input.txt"
  let sums = sumEachList lists
  print sums
  let maxSum = maximum sums
  print maxSum
  let topThreeSum = findAndSumTopThree lists
  print topThreeSum
