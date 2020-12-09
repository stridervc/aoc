import Data.List (tails)

-- take a number and a list of numbers
-- the number is valid if any two of the numbers from the list add up to it
valid :: [Int] -> Int -> Bool
valid ns n = not $ null [(x,y) | (x:ys) <- tails ns, y <- ys, x+y==n]

-- check if number at pos i of list is valid, considering
-- preamble numbers before it
validAtPos :: [Int] -> Int -> Int -> Bool
validAtPos ns preamble i = valid ns' $ ns!!i
  where ns' = take preamble $ reverse $ take i ns

-- find the first invalid number in the list, using preamble
firstInvalid :: [Int] -> Int -> Int
firstInvalid ns preamble = head $ [ns!!i | i <- [preamble..], not $ validAtPos ns preamble i]

-- return a list of overlapping contiguous subsets of input list, of length n each
subsets :: [Int] -> Int -> [[Int]]
subsets ns n = map ssf drops
  where drops = [0..length ns - n]
        ssf x = take n $ drop x ns

-- find the first subset of contiguous numbers in list that add up to n
findSet :: [Int] -> Int -> [Int]
findSet ns n = head $ [ss | ss <- subsets', sum ss == n]
  where subsets'  = concat $ map (subsets ns) [2..]

main = do
  input <- map read <$> lines <$> readFile "input9.txt"

  -- Part One --
  let invalidNum = firstInvalid input 25
  print invalidNum

  -- Part Two --
  let set = findSet input invalidNum
  print $ (minimum set) + (maximum set)
