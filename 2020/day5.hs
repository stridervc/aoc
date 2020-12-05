import Data.List (sort)

type BPass  = String
type SeatID = Int

-- binary string to int
binToInt :: String -> Int
binToInt []       = 0
binToInt ('1':xs) = (2^(length xs)) + binToInt xs
binToInt (_:xs)   = binToInt xs

-- boarding pass seat ID
seatID :: BPass -> SeatID
seatID bp = binToInt binstr
  where binstr  = map (\c -> if (c == 'B' || c == 'R') then '1' else '0') bp

-- needs seat ids to be sorted, finds first missing seat
findMySeat :: [SeatID] -> SeatID
findMySeat (x:xs)
  | x+1 == head xs  = findMySeat xs
  | otherwise       = x + 1

main = do
  ids <- map seatID <$> lines <$> readFile "input5.txt"

  -- Part One --
  print $ maximum ids

  -- Part Two --
  print $ findMySeat $ sort ids
