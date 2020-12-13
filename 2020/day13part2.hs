import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Bus  = Maybe Integer
type Time = Integer

parseInput :: String -> [Bus]
parseInput input  = map (\b -> if b == "x" then Nothing else Just $ read b) $ splitOn "," busline
  where input'    = lines input
        busline   = input'!!1

-- will this bus depart at this time?
busDepartsAt :: Bus -> Time -> Bool
busDepartsAt Nothing    _ = True
busDepartsAt (Just bus) t = t `mod` bus == 0

-- give a list of buses and a time
-- returns true iff bus 0 departs at time, and bus 1 departs at time + 1, etc
validStaggered :: [Bus] -> Time -> Bool
validStaggered []     _ = True
validStaggered (b:bs) t = busDepartsAt b t && validStaggered bs (t+1)

-- give list of buses, a time to start at, an amount to increment by and a count of
-- how many buses from the list to use
-- gives back a new start time and increment (product of buses)
solve :: [Bus] -> (Time, Integer) -> Int -> (Time, Integer)
solve buses (start, incr) count  = (newstart, newincr)
  where buses'    = take count buses
        potential = iterate (+incr) start
        newstart  = head [t | t <- potential, validStaggered buses' t]
        newincr   = product $ catMaybes buses'

-- start with first 2 buses and 0, 1 for start and incr
-- find a new start and incr
-- go on to 3 buses with new start and incr
-- until all buses, then answer is start
part2 :: [Bus] -> Integer
part2 buses = fst $ foldl (solve buses) (0,1) [2..n]
  where n   = length buses

main :: IO ()
main = do
  input <- parseInput <$> readFile "input13.txt"

  print $ part2 input
