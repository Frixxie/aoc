module Main (main) where

import qualified Data.Maybe

class ToValue a where
  toValue :: a -> Int

data RPS = Rock | Paper | Scissors deriving (Show)

data Outcome = Win | Lose | Draw deriving (Show)

instance ToValue RPS where
  toValue Rock = 1
  toValue Paper = 2
  toValue Scissors = 3

instance ToValue Outcome where
  toValue Win = 6
  toValue Draw = 3
  toValue Lose = 0

calcGame :: RPS -> RPS -> Int
calcGame Rock Scissors = toValue Scissors + toValue Lose
calcGame Rock Paper = toValue Paper + toValue Win
calcGame Rock Rock = toValue Rock + toValue Draw
calcGame Paper Rock = toValue Rock + toValue Lose
calcGame Paper Scissors = toValue Scissors + toValue Win
calcGame Paper Paper = toValue Paper + toValue Draw
calcGame Scissors Paper = toValue Paper + toValue Lose
calcGame Scissors Rock = toValue Rock + toValue Win
calcGame Scissors Scissors = toValue Scissors + toValue Draw

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
