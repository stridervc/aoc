type Cell       = Char
type Field      = [[Cell]]
type Pos        = (Int, Int)
type Direction  = (Int, Int)

-- Type definition for a function that takes a read only field, a writeable
-- field and a cell position. It returns the original read only field, and a
-- new field with the cell iterated
type IterCellFunc = (Field, Field) -> Pos -> (Field, Field)

-- return list of neighbours for cell at position
neighbours :: Field -> Pos -> [Cell]
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

-- part 2
-- first seat in direction, or floor if none
firstSeat :: Field -> Pos -> Direction -> Cell
firstSeat field (x,y) (dx,dy)
  | x == 0 && dx == -1  = '.'
  | y == 0 && dy == -1  = '.'
  | x == w && dx == 1   = '.'
  | y == h && dy == 1   = '.'
  | nc /= '.'           = nc
  | otherwise           = firstSeat field (nx,ny) (dx,dy)
  where w             = length (row 0) - 1
        h             = length field - 1
        row r         = field !! r
        (nx,ny)       = (x+dx, y+dy)
        cell (x',y')  = row y' !! x'
        nc            = cell (nx, ny)

-- part 2 neighbours
neighbours2 :: Field -> Pos -> [Cell]
neighbours2 field pos = [ cn, cne, ce, cse, cs, csw, cw, cnw ]
  where nc  = firstSeat field pos
        cn  = nc ( 0, -1 )
        cne = nc ( 1, -1 )
        ce  = nc ( 1, 0  )
        cse = nc ( 1, 1  )
        cs  = nc ( 0, 1  )
        csw = nc (-1, 1  )
        cw  = nc (-1, 0  )
        cnw = nc (-1, -1 )

-- count occupied seats neighbouring position
occupiedNeighbours :: Field -> Pos -> Int
occupiedNeighbours field pos = length $ filter (== '#') $ neighbours field pos

-- part 2 occupied neighbours
occupiedNeighbours2 :: Field -> Pos -> Int
occupiedNeighbours2 field pos = length $ filter (== '#') $ neighbours2 field pos

-- replace cell at position
replaceCell :: Field -> Pos -> Cell -> Field
replaceCell field (x,y) cell = prerows ++ [precells ++ [cell] ++ postcells] ++ postrows
  where prerows   = take y field
        precells  = take x $ field !! y
        postcells = drop (x+1) $ field !! y
        postrows  = drop (y+1) field

-- pass an original field to read from,
-- and a 'writeable' field that may change
-- returns the same
iterCell :: IterCellFunc
iterCell (rfield,wfield) (x,y)
  | c == 'L' && oc == 0 = (rfield, replaceCell wfield (x,y) '#')
  | c == '#' && oc >= 4 = (rfield, replaceCell wfield (x,y) 'L')
  | otherwise           = (rfield, wfield)
  where c   = rfield !! y !! x
        oc  = occupiedNeighbours rfield (x,y)

-- part 2 iterCell
iterCell2 :: IterCellFunc
iterCell2 (rfield,wfield) (x,y)
  | c == 'L' && oc == 0 = (rfield, replaceCell wfield (x,y) '#')
  | c == '#' && oc >= 5 = (rfield, replaceCell wfield (x,y) 'L')
  | otherwise           = (rfield, wfield)
  where c   = rfield !! y !! x
        oc  = occupiedNeighbours2 rfield (x,y)

-- do one iteration of whole field, using given iteration function
iterField :: Field -> IterCellFunc -> Field
iterField field func = snd $ foldl func (field, field) poss
  where poss  = [(x,y) | x <- [0..w], y <- [0..h]]
        w     = length (head field) - 1
        h     = length field - 1

-- keep iterating field until it doesn't change, using iteration function
iterUntilStable :: Field -> IterCellFunc -> Field
iterUntilStable field func
  | field == field' = field
  | otherwise       = iterUntilStable field' func
  where field'      = iterField field func

{-
printField :: Field -> IO ()
printField = mapM_ putStrLn
-}

-- iterate field until stable, then count the number of occupied seats
occupiedWhenStable :: Field -> IterCellFunc -> Int
occupiedWhenStable field func = sum $ map (length . filter (== '#')) $ iterUntilStable field func

part1 :: Field -> Int
part1 field = occupiedWhenStable field iterCell

part2 :: Field -> Int
part2 field = occupiedWhenStable field iterCell2

main :: IO ()
main = do
  field <- lines <$> readFile "input11.txt"
  print $ part1 field
  print $ part2 field
