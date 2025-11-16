module Main (main) where

newtype Stack a = Stack [a] deriving (Show)

emptyStack :: Stack a
emptyStack = Stack []

pushStack :: a -> Stack a -> Stack a
pushStack x (Stack xs) = Stack (x : xs)

popStack :: Stack a -> Maybe (a, Stack a)
popStack (Stack []) = Nothing
popStack (Stack (x : xs)) = Just (x, Stack xs)

peekStack :: Stack a -> Maybe a
peekStack (Stack []) = Nothing
peekStack (Stack (x : _)) = Just x

--     [D]
-- [N] [C]
-- [Z] [M] [P]
--  1   2   3
parseInput :: String -> IO [Stack Char]
parseInput input = return [Stack "NZ", Stack "DCM", Stack "P"]

newtype Move = Move (Int, Int, Int) deriving (Show)

-- move 1 from 2 to 1
-- move 3 from 1 to 3
-- move 2 from 2 to 1
-- move 1 from 1 to 2
parseMoves :: String -> IO [Move]
parseMoves input = return [Move (1, 2, 1), Move (3, 1, 3), Move (2, 2, 1), Move (1, 1, 2)]

applyMove :: Move -> [Stack Char] -> [Stack Char]
applyMove (Move (n, from, to)) s =
    let (Stack fromStack) = s !! (from - 1)
        (Stack toStack) = s !! (to - 1)
        (movedItems, remainingFromStack) = splitAt n fromStack
        newToStack = movedItems ++ toStack
        newStacks = take (from - 1) s ++ [Stack remainingFromStack] ++ drop from s
    in take (to - 1) newStacks ++ [Stack newToStack] ++ drop to newStacks

main :: IO ()
main = do
  stacks <- parseInput ""
  moves <- parseMoves ""
  print stacks
  print moves
  let finalStacks = foldl (flip applyMove) stacks moves
  print finalStacks
