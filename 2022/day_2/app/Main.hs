module Main (main) where

import qualified Data.Maybe

data RPS = Rock | Paper | Scissors deriving (Show)

calcGame :: RPS -> RPS -> Int
calcGame Rock Scissors = 3
calcGame Rock Paper = 8
calcGame Rock Rock = 4
calcGame Paper Rock = 1
calcGame Paper Scissors = 9
calcGame Paper Paper = 5
calcGame Scissors Paper = 2
calcGame Scissors Rock = 7
calcGame Scissors Scissors = 6

interpretGame :: String -> Maybe (RPS, RPS)
interpretGame "A X" = Just (Rock, Rock)
interpretGame "A Y" = Just (Rock, Paper)
interpretGame "A Z" = Just (Rock, Scissors)
interpretGame "B X" = Just (Paper, Rock)
interpretGame "B Y" = Just (Paper, Paper)
interpretGame "B Z" = Just (Paper, Scissors)
interpretGame "C X" = Just (Scissors, Rock)
interpretGame "C Y" = Just (Scissors, Paper)
interpretGame "C Z" = Just (Scissors, Scissors)
interpretGame _ = Nothing

interpretGame' :: String -> Maybe (RPS, RPS)
interpretGame' "A X" = Just (Rock, Scissors)
interpretGame' "A Y" = Just (Rock, Rock)
interpretGame' "A Z" = Just (Rock, Paper)
interpretGame' "B X" = Just (Paper, Rock)
interpretGame' "B Y" = Just (Paper, Paper)
interpretGame' "B Z" = Just (Paper, Scissors)
interpretGame' "C X" = Just (Scissors, Paper)
interpretGame' "C Y" = Just (Scissors, Scissors)
interpretGame' "C Z" = Just (Scissors, Rock)
interpretGame' _ = Nothing

readGames :: String -> IO [(RPS, RPS)]
readGames input = do
  content <- readFile input
  let linesOfInput = lines content
  let games = map interpretGame' linesOfInput
  return (Data.Maybe.catMaybes games)

main :: IO ()
main = do
  games <- readGames "input.txt"
  print games
  let totalScore = sum [calcGame opponent player | (opponent, player) <- games]
  print totalScore
