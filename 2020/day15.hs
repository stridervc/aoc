import AoC

import Data.List (elemIndex)

next :: [Int] -> [Int]
next l@(x:xs)
  | isnew     = 0 : l
  | otherwise = index + 1 : l
  where isnew       = x `notElem` xs
        Just index  = elemIndex x xs

nextUntilLength :: Int -> [Int] -> [Int]
nextUntilLength len nums
  | length nums < len = nextUntilLength len $ next nums
  | otherwise         = nums

part1 = head . nextUntilLength 2020
part2 = head . nextUntilLength 30000000

main :: IO ()
main = do
  let example1  = reverse [0,3,6]
  let example2  = reverse [1,3,2]
  let input     = reverse [0,1,5,10,3,12,19]

  putStrLn "Part 1"
  testAndRun_ part1 [(example1, 436), (example2, 1)] input

  -- print $ reverse $ nextUntilLength 10000 example1

  putStrLn ""
  putStrLn "Part 2"
  print $ part2 input
  {-
  testAndRun_ part2 [(example1, 175594), (example2, 2578)] input
  -}
