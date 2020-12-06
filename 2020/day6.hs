import Data.List (nub, sort, group)
import Data.List.Split (splitOn)

type Group  = [String]

-- count each 'yes' answer
countDistinct :: Group -> Int
countDistinct g = length $ nub $ concat g

-- count answers where all answered 'yes'
countAll :: Group -> Int
countAll g = length $ filter (\i -> length i == n) $ group $ sort $ concat g
  where n = length g

main = do
  groups <- map lines <$> splitOn "\n\n" <$> readFile "input6.txt"

  -- Part One --
  print $ sum $ map countDistinct groups

  -- Part Two --
  print $ sum $ map countAll groups
