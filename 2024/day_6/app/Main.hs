module Main where

import qualified Data.Map as Map

data Direction = U | L | R | D deriving (Show)

data Spot = Path | Obstacle | Guard Direction | Visited deriving (Show)

data Coord = Coord Int Int deriving (Show, Ord, Eq)

data Grid = Tile Coord Spot deriving (Show)

rotateGuard :: Spot -> Spot
rotateGuard (Guard U) = Guard R
rotateGuard (Guard R) = Guard D
rotateGuard (Guard D) = Guard L
rotateGuard (Guard L) = Guard U
rotateGuard s = s

contentToGrid :: String -> Int -> Int -> [Grid]
contentToGrid ('\n' : xs) row _ = contentToGrid xs (row + 1) 0
contentToGrid ('.' : xs) row col = Tile (Coord row col) Path : contentToGrid xs row (col + 1)
contentToGrid ('#' : xs) row col = Tile (Coord row col) Obstacle : contentToGrid xs row (col + 1)
contentToGrid ('^' : xs) row col = Tile (Coord row col) (Guard U) : contentToGrid xs row (col + 1)
contentToGrid [] _ _ = []
contentToGrid (_ : _) _ _ = error "invalid char"

gridToMap :: [Grid] -> Map.Map Coord Spot -> Map.Map Coord Spot
gridToMap [] m = m
gridToMap (Tile (Coord row col) spot : xs) m = gridToMap xs (Map.insert (Coord row col) spot m)

main :: IO ()
main = do
  content <- readFile "sample_input.txt"
  let grid = contentToGrid content 0 0
  let gridMap = gridToMap grid Map.empty
  print gridMap
