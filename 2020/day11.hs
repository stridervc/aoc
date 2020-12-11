type Cell   = Char
type Field  = [[Cell]]

neighbours :: Field -> (Int, Int) -> [Cell]
neighbours field (x,y)
  | x == 0 && y == 0  = [ ce, cse, cs ]
  | x == w && y == 0  = [ cs, csw, cw ]
  | x == 0 && y == h  = [ cn, cne, ce ]
  | x == w && y == h  = [ cn, cnw, cw ]
  | x == 0            = [ cn, cne, ce, cse, cs ]
  | x == w            = [ cn, cnw, cw, csw, cs ]
  | y == 0            = [ ce, cse, cs, csw, cw ]
  | y == h            = [ cn, cne, ce, cnw, cw ]
  | otherwise         = [ cn, cne, ce, cse, cs, csw, cw, cnw ]
  where w             = length (row 0) - 1
        h             = length field - 1
        cell (x',y')  = row y' !! x'
        row r         = field !! r
        cn            = cell (x  , y-1)
        cne           = cell (x+1, y-1)
        ce            = cell (x+1, y  )
        cse           = cell (x+1, y+1)
        cs            = cell (x  , y+1)
        csw           = cell (x-1, y+1)
        cw            = cell (x-1, y  )
        cnw           = cell (x-1, y-1)

occupiedNeighbours :: Field -> (Int, Int) -> Int
occupiedNeighbours field pos = length $ filter (== '#') $ neighbours field pos

replaceCell :: Field -> (Int, Int) -> Cell -> Field
replaceCell field (x,y) cell = prerows ++ [precells ++ [cell] ++ postcells] ++ postrows
  where prerows   = take y field
        precells  = take x $ field !! y
        postcells = drop (x+1) $ field !! y
        postrows  = drop (y+1) field

-- pass it an original field to keep unchanged
-- and a 'writeable' field
-- returns the same
iterCell :: (Field, Field) -> (Int, Int) -> (Field, Field)
iterCell (rfield,wfield) (x,y)
  | c == 'L' && oc == 0 = (rfield, replaceCell wfield (x,y) '#')
  | c == '#' && oc >= 4 = (rfield, replaceCell wfield (x,y) 'L')
  | otherwise           = (rfield, wfield)
  where c   = rfield !! y !! x
        oc  = occupiedNeighbours rfield (x,y)

iterField :: Field -> Field
iterField field = snd $ foldl iterCell (field, field) poss
  where poss  = [(x,y) | x <- [0..w], y <- [0..h]]
        w     = length (head field) - 1
        h     = length field - 1

iterUntilStable :: Field -> Field
iterUntilStable field
  | field == field' = field
  | otherwise       = iterUntilStable field'
  where field'      = iterField field

{-
printField :: Field -> IO ()
printField = mapM_ putStrLn
-}

part1 :: Field -> Int
part1 field = sum $ map (length . filter (== '#')) $ iterUntilStable field

main :: IO ()
main = do
  field <- lines <$> readFile "input11.txt"
  print $ part1 field
