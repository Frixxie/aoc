module Main where

import qualified Data.Bifunctor
import Data.Either
import Text.Parsec

data Mul = Mul Int Int | Do | Dont deriving (Show)

parseMul' :: Parsec String () Mul
parseMul' = do
  _ <- string "mul"
  _ <- char '('
  left <- many digit
  _ <- char ','
  right <- many digit
  _ <- char ')'
  return $ Mul (read left) (read right)

parseDo :: Parsec String () Mul
parseDo = do
  _ <- string "do()"
  return Do

parseDont :: Parsec String () Mul
parseDont = do
  _ <- string "don't()"
  return Dont

parseAll :: Parsec String () Mul
parseAll = try parseMul' <|> try parseDo <|> parseDont

findAllMul' :: String -> [Either ParseError Mul]
findAllMul' "" = []
findAllMul' s = parse parseAll "" s : findAllMul' (drop 1 s)

parseMul :: Parsec String () ([Char], [Char])
parseMul = do
  _ <- string "mul"
  _ <- char '('
  left <- many digit
  _ <- char ','
  right <- many digit
  _ <- char ')'
  return (left, right)

findAllMul :: String -> [Either ParseError ([Char], [Char])]
findAllMul "" = []
findAllMul s = parse parseMul "" s : findAllMul (drop 1 s)

convertToDigit :: [Either ParseError ([Char], [Char])] -> [(Int, Int)]
convertToDigit xs = map (Data.Bifunctor.bimap read read) $ rights xs

sumMuls :: Mul -> [Mul] -> Int
sumMuls Do (Mul a b : xs) = a * b + sumMuls Do xs
sumMuls Dont (Mul _ _ : xs) = sumMuls Dont xs
sumMuls _ (Do : xs) = sumMuls Do xs
sumMuls _ (Dont : xs) = sumMuls Dont xs
sumMuls _ _ = 0

main :: IO ()
main = do
  contents <- readFile "input.txt"

  -- Part I
  let digits = convertToDigit $ findAllMul contents
  let result = foldr (\(a, b) acc -> acc + a * b) 0 digits
  print result

  -- Part II
  let muls = rights $ findAllMul' contents
  print $ sumMuls Do muls
