import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.List (elemIndex)

type Bus  = Maybe Integer
type Time = Integer

parseInput :: String -> [Bus]
parseInput input = busses
  where input'    = lines input
        busses    = map (\b -> if b == "x" then Nothing else Just $ read b) $ splitOn "," $ head $ tail input'

schedule :: Bus -> [Time]
schedule Nothing    = []
schedule (Just bus) = iterate (+bus) 0

busDepartsAt :: Bus -> Time -> Bool
busDepartsAt Nothing    _ = True
busDepartsAt (Just bus) t = t `mod` bus == 0

validStaggered :: [Bus] -> Integer -> Time -> Bool
validStaggered []     _      _  = True
validStaggered (b:bs) offset t  = busDepartsAt b (t-offset) && validStaggered bs offset (t+1)

-- get position in list of slowest bus
slowestBus :: [Bus] -> (Integer, Bus)
slowestBus busses = (toInteger pos, Just bus)
  where bus       = maximum $ catMaybes busses
        Just pos  = elemIndex (Just bus) busses

part2 :: [Bus] -> Integer
part2 busses = snd (head $ dropWhile (not . fst) vsst) - pos
  where (pos, bus)  = slowestBus busses
        potential   = schedule bus
        vss         = map (validStaggered busses pos) potential
        vsst        = zip vss potential

main :: IO ()
main = do
  input <- parseInput <$> readFile "input13.txt"

  print $ part2 input
