import Data.List.Split (splitOn)

type Bus  = Maybe Int
type Time = Int

parseInput :: String -> [Bus]
parseInput input = busses
  where input'    = lines input
        busses    = map (\b -> if b == "x" then Nothing else Just $ read b) $ splitOn "," $ head $ tail input'

schedule :: Bus -> [Time]
schedule Nothing    = []
schedule (Just bus) = iterate (+bus) 0

busDepartsAt :: Bus -> Time -> Bool
busDepartsAt Nothing _    = True
busDepartsAt (Just bus) t = t `elem` schedule (Just bus)

validStaggered :: [Bus] -> Time -> Bool
validStaggered [] _     = True
validStaggered (b:bs) t = busDepartsAt b t && validStaggered bs (t+1)

part2 :: [Bus] -> Int
part2 busses = snd $ head $ dropWhile (not . fst) $ zip (map (validStaggered busses) potential) potential
  where potential = schedule $ head busses

main :: IO ()
main = do
  input <- parseInput <$> readFile "example13.txt"

  print $ part2 input
