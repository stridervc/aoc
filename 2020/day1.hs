import System.IO
import Data.List

-- take a list of numbers, return the first two numbers
-- that add up to 2020
sumTo2020 :: [Int] -> [(Int,Int)]
sumTo2020 xs = take 1 $ filter (\(x,y) -> x + y == 2020) pairs
  where pairs = [(x,y) | (x:ys) <- tails xs, y <- ys]

-- take a list of numbers, return the first three numbers
-- that add up to 2020
tripleTo2020 :: [Int] -> [(Int,Int,Int)]
tripleTo2020 xs = take 1 $ filter (\(x,y,z) -> x + y + z == 2020) triples
  where triples = [(x,y,z) | (x:ys) <- tails xs, (y:ys2) <- tails ys, z <- ys2]

tripleMul :: (Int,Int,Int) -> Int
tripleMul (a,b,c) = a * b * c

main = do
  -- let nums = [979,366,675,1456,1721,299]

  contents <- readFile "input1.txt"
  let nums = map read $ lines contents

  let res   = (sumTo2020 nums)!!0
  let res3  = (tripleTo2020 nums)!!0

  putStrLn $ show res
  putStrLn $ show $ (fst res) * (snd res)

  putStrLn $ show res3
  putStrLn $ show $ tripleMul res3
