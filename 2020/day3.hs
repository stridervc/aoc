type Pos    = (Int,Int)
type Move   = (Int,Int)
type Strat  = (Int,Int)
type Topo   = [[Char]]

applyMove :: Pos -> Move -> Pos
applyMove (px,py) (mx,my) = (px+mx,py+my)

-- return what's at pos on map, or 'X' if off bottom of map
whatsHere :: Topo -> Pos -> Char
whatsHere topo (x,y)
  | y >= length topo  = 'X'
  | otherwise         = topo!!y!!x'
  where x'  = x `mod` length (topo!!y)

-- how many trees do we hit with given movement strategy
treesHit :: Topo -> Strat -> Int
treesHit topo strat = length $ filter (=='#') whatss
  where poss    = scanl applyMove (0,0) $ repeat strat
        whatss  = takeWhile (/='X') $ map (whatsHere topo) poss

main = do
  topo <- lines <$> readFile "input3.txt"

  --- Part One ---
  putStrLn $ show $ treesHit topo (3,1)

  --- Part Two ---
  let strats = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  let hits = map (treesHit topo) strats

  putStrLn $ show $ foldl1 (*) hits
