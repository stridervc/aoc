import AoC

import Data.List (elemIndex)

import qualified Data.Map as M

type Memo = M.Map Int Int

data Game = Game
  { latest  :: Int
  , memo    :: M.Map Int Int
  , numlen  :: Int
  } deriving (Eq, Show)

newGame :: [Int] -> Game
newGame nums =
  Game  { latest  = last nums
        , memo    = M.fromList $ zip (init nums) [0..]
        , numlen  = length nums - 1
        }

next :: Game -> Game
next game =
  Game  { latest  = nextnum
        , memo    = newmemo
        , numlen  = len + 1
        }
  where memo'   = memo game
        latest' = latest game
        len     = numlen game
        newmemo = M.insert latest' len memo'
        nextnum = case M.lookup latest' memo' of
          Nothing     -> 0
          Just index  -> len - index

nextUntilLength :: Int -> Game -> Game
nextUntilLength len game
  | len' < len  = nextUntilLength len $ next game
  | otherwise   = game
  where len'    = numlen game + 1

solve' :: Int -> [Int] -> Game
solve' count nums = nextUntilLength count $ newGame nums

solve :: Int -> [Int] -> Int
solve count nums = latest $ solve' count nums

part1 = solve 2020
part2 = solve 30000000

main :: IO ()
main = do
  let example1  = [0,3,6]
  let example2  = [1,3,2]
  let example3  = [2,1,3]
  let input     = [0,1,5,10,3,12,19]

  putStrLn "Part 1"
  testAndRun_ part1 [(example1, 436), (example2, 1), (example3, 10)] input

  print $ solve' 7 example1

  {-
  putStrLn ""
  putStrLn "Part 2"
  print $ part2 input
  testAndRun_ part2 [(example1, 175594), (example2, 2578)] input
  -}
