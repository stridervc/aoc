import Data.List (nub, sort, group)
import Data.List.Split (splitOn)

type Group  = [String]

-- count each 'yes' answer
countDistinct :: Group -> Int
countDistinct = length . nub . concat

-- count answers where all answered 'yes'
countAll :: Group -> Int
countAll g = length $ filter allyes $ group $ sort $ concat g
  where numppl  = length g
        allyes  = (\anss -> length anss == numppl)

main = do
  groups <- map lines <$> splitOn "\n\n" <$> readFile "input6.txt"

  -- Part One --
  print $ sum $ map countDistinct groups

  -- Part Two --
  print $ sum $ map countAll groups
