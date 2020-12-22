import AoC

import Data.List.Split (splitOn)

parseInput :: String -> ([Int], [Int])
parseInput input = (p1, p2)
  where input'  = map (splitOn "\n") $ splitOn "\n\n" input
        p1      = map read $ tail $ head input'
        p2      = map read $ tail $ init $ input' !! 1

-- true iff game done
isDone :: ([Int], [Int]) -> Bool
isDone (p1, p2) = null p1 || null p2

-- play one round of the game
play :: ([Int], [Int]) -> ([Int], [Int])
play decks@(p1, p2)
  | isDone decks  = decks
  | otherwise     = if p1c > p2c then (p1w, p2l) else (p1l, p2w)
  where p1c = head p1
        p2c = head p2
        p1w = tail p1 ++ [p1c,p2c]
        p1l = tail p1
        p2w = tail p2 ++ [p2c,p1c]
        p2l = tail p2

-- play until someone runs out of cards
playUntilEnd :: ([Int], [Int]) -> ([Int], [Int])
playUntilEnd decks
  | isDone decks  = decks
  | otherwise     = playUntilEnd $ play decks

-- return the winning player's score
score :: ([Int], [Int]) -> Int
score (p1, p2)
  | null p1   = score' p2
  | otherwise = score' p1
  where score' []     = 0
        score' (c:cs) = c * (length cs + 1) + score' cs

part1 :: String -> Int
part1 = score . playUntilEnd . parseInput

main :: IO ()
main = do
  example <- readFile "example22.txt"
  input   <- readFile "input22.txt"

  print $ part1 example
  print $ part1 input
