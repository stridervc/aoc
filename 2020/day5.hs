type BPass  = String
type Row    = Int
type Col    = Int

-- Return new lower and upper bounds, given starting bounds, chars that specify each, and the
-- char to apply
-- (Lower half char, higher half char) -> (Lower bound, Upper bound) -> Char to apply -> Result
half :: (Char,Char) -> (Int,Int) -> Char -> (Int,Int)
half (lc,hc) (l,h) c
  | c == lc   = (l, l+halve-1)
  | c == hc   = (l+halve, h)
  where halve = (h-l+1) `div` 2

narrowDown' :: (Char,Char) -> (Int,Int) -> String -> (Int,Int)
narrowDown' _   bounds []     = bounds
narrowDown' chs bounds (x:xs) = narrowDown' chs bounds' xs
  where bounds' = half chs bounds x

narrowDown :: (Char,Char) -> String -> Int
narrowDown chs xs = fst $ narrowDown' chs (0,hi) xs
  where hi  = 2 ^ (length xs) - 1

seat :: BPass -> (Row,Col)
seat bp = (row,col)
  where rowstr  = take 7 bp
        colstr  = drop 7 bp
        row     = narrowDown ('F','B') rowstr
        col     = narrowDown ('L','R') colstr

seatID :: (Row,Col) -> Int
seatID (r,c) = r*8+c

main = do
  passes <- lines <$> readFile "input5.txt"

  -- Part One --
  print $ maximum $ map (seatID . seat) passes

