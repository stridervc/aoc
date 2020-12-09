import Data.List (tails)

-- take a number and a list of numbers
-- the number is valid if two of the numbers from the list add up to it
valid :: Int -> [Int] -> Bool
valid n ns = not $ null [(x,y) | (x:ys) <- tails ns, y <- ys, x+y==n]

validAtPos :: [Int] -> Int -> Int -> Bool
validAtPos ns preamble i = valid n ns'
  where n   = ns!!i
        ns' = take preamble $ reverse $ take i ns

firstInvalid :: [Int] -> Int -> Int
firstInvalid ns preamble = head $ [x | i <- poss, let x = ns!!i, validAtPos ns preamble i == False]
  where poss  = enumFrom preamble

-- findSet :: [Int] -> Int -> [Int]

main = do
  input <- map read <$> lines <$> readFile "input9.txt"

  print $ firstInvalid input 25
