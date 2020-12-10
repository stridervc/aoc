import Data.List (sort)

addEndPoints is = 0 : is ++ [maximum is + 3]

count :: [Int] -> Int -> Int
count is e = length $ filter (==e) is

countGaps :: [Int] -> [Int]
countGaps is = map (count gaps) [0..3]
  where gaps  = [is!!(i+1)-is!!i | i <- [0..length is - 2]]

part1 :: [Int] -> Int
part1 input = gaps!!1 * gaps!!3
  where input'  = addEndPoints $ sort input
        gaps    = countGaps input'

-- list of next valid adapters following 'j'
nextValids :: [Int] -> Int -> [Int]
nextValids js j = [n | n <- js, n-j>0, n-j<=3]

main = do
  input <- map read <$> lines <$> readFile "input10.txt"
  -- input <- map read <$> lines <$> readFile "example10.txt"

  print $ part1 input

