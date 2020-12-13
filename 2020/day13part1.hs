import Data.List.Split (splitOn)
import Data.List (sortOn)

import AoC

type Bus  = Int
type Time = Int

schedule :: Bus -> [(Bus,Time)]
schedule bus = zip (repeat bus) $ iterate (+bus) 0

parseInput :: String -> (Time, [Bus])
parseInput input = (earliest, busses)
  where input'    = lines input
        earliest  = read $ head input'
        busses    = map read $ filter (/= "x") $ splitOn "," $ head $ tail input'

earliestTime :: Time -> Bus -> (Bus,Time)
earliestTime t bus = head $ dropWhile (\bt -> snd bt < t) $ schedule bus

part1 :: (Time, [Bus]) -> Int
part1 (earliest, busses) = (bustime - earliest) * bus
  where bustimes  = sortOn snd $ map (earliestTime earliest) busses
        bustime   = snd $ head bustimes
        bus       = fst $ head bustimes

main :: IO ()
main = do
  example <- parseInput <$> readFile "example13.txt"
  input   <- parseInput <$> readFile "input13.txt"

  testAndRun_ part1 [(example,295)] input
