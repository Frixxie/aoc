module Main (main) where

isInvalidId :: String -> String -> Bool
isInvalidId [] [] = True
isInvalidId [] _ = False
isInvalidId _ [] = False
isInvalidId (x : xs) (y : ys)
  | x /= y = False
  | otherwise = isInvalidId xs ys

checkInvalid :: String -> Bool
checkInvalid rateId =
  let (start, end) = splitAt (length rateId `div` 2) rateId
   in isInvalidId start end

genRange :: (Int, Int) -> [String]
genRange (startId, endId) =
  [show n | n <- [startId .. endId]]

parseRateIdRange :: String -> (Int, Int)
parseRateIdRange rangeStr =
  let (startStr, endStr) = break (== '-') rangeStr
      startId = read startStr :: Int
      endId = read (tail endStr) :: Int
   in (startId, endId)

splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma str =
  let (first, rest) = break (== ',') str
   in first : case rest of
        [] -> []
        (_ : xs) -> splitOnComma xs

parseRateIdList :: String -> [String]
parseRateIdList = splitOnComma

readInput :: String -> IO [(Int, Int)]
readInput filePath = do
  contents <- readFile filePath
  let ranges = parseRateIdList contents
  return $ map parseRateIdRange ranges

main :: IO ()
main = do
  let filePath = "test_input.txt"
  ranges <- readInput filePath
  let all_ranges = map genRange ranges
  let all_strings = concat all_ranges
  let invalid = filter checkInvalid all_strings
  let invalid_ids = map read invalid :: [Int]
  print $ sum invalid_ids
