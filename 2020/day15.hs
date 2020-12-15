import AoC

import Data.List (elemIndex)

import qualified Data.Map.Strict as M

type Memo = M.Map Int Int

next :: (Memo, [Int]) -> (Memo, [Int])
next (memo, l@(x:xs)) =
  case M.lookup x memo of
    Nothing     -> if isnew then (memo', 0 : l) else (memo', index+1 : l)
    Just index  -> (memo', (length xs - index) : l)
    where isnew       = x `notElem` xs
          Just index  = x `elemIndex` xs
          memo'       = M.insert x (length xs) memo

nextUntilLength :: Int -> (Memo, [Int]) -> (Memo, [Int])
nextUntilLength len (memo, nums)
  | length nums < len = nextUntilLength len $ next (memo, nums)
  | otherwise         = (memo, nums)

solve' :: Int -> [Int] -> (Memo, [Int])
solve' count nums = nextUntilLength count (M.empty, nums)

solve :: Int -> [Int] -> Int
solve count nums = head $ snd $ solve' count nums

part1 = solve 2020
part2 = solve 30000000

main :: IO ()
main = do
  let example1  = reverse [0,3,6]
  let example2  = reverse [1,3,2]
  let example3  = reverse [2,1,3]
  let input     = reverse [0,1,5,10,3,12,19]

  putStrLn "Part 1"
  testAndRun_ part1 [(example1, 436), (example2, 1), (example3, 10)] input

  -- print $ solve' 7 example1

  putStrLn ""
  putStrLn "Part 2"
  print $ part2 input
  testAndRun_ part2 [(example1, 175594), (example2, 2578)] input
