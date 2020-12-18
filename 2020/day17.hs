import AoC

import Control.Monad (when)
import Data.List.Split (chunksOf)
import Text.Printf (printf)

type ZSlice = [[Bool]]
type Coord  = (Int, Int, Int)

data ConwayCube = ConwayCube
  { cycles  :: !Int      -- the number of cycles this cube will support, determines max size
  , current :: !Int      -- current cycle
  , slices  :: ![ZSlice]
  , width   :: !Int
  , height  :: !Int
  , depth   :: !Int
  } deriving (Eq, Show)

newConwayCube :: Int -> [String] -> ConwayCube
newConwayCube cycles input = ConwayCube
  { cycles  = cycles
  , current = 0
  , slices  = replicate cycles blankslice ++ [startslice] ++ replicate cycles blankslice
  , width   = w
  , height  = h
  , depth   = d
  }
  where blankslice  = replicate h $ replicate w False
        iw          = length $ head input
        ih          = length input
        w           = iw + 2*cycles
        h           = ih + 2*cycles
        d           = 1  + 2*cycles
        -- startslice needs padding on all 4 sides
        tempslice   = map (map (== '#')) input
        padrows     = replicate cycles $ replicate w False
        padslice    = map (\r -> replicate cycles False ++ r ++ replicate cycles False) tempslice
        startslice  = padrows ++ padslice ++ padrows

-- just for debugging
printConwayCube :: ConwayCube -> IO ()
printConwayCube cube = do
  printf "Cycle %d / %d\n" (current cube) (cycles cube)
  -- printf "Slices = %d\n\n" (length $ slices cube)
  mapM_ printSlice $ zip [0..] $ slices cube
  where printSlice (z, slice)  = when (or $ concat slice) (do
                                  putStr "z = "
                                  print $ z - cycles cube
                                  mapM_ (putStrLn . map (\c -> if c then '#' else '.')) slice
                                  )

-- we can work with (0,0,0) being the top left of the slice we give newConwayCube
-- this gives the coords as ConwayCube uses them
{-
actualCoords :: ConwayCube -> (Int, Int, Int) -> (Int, Int, Int)
actualCoords cube (x,y,z) = (x+c, y+c, z+c)
  where c = cycles cube
-}

getXYZ :: ConwayCube -> Coord -> Bool
getXYZ cube (x,y,z) = slice !! y !! x
  where slice   = slices cube !! z
        w       = width cube

setXYZ :: ConwayCube -> Coord -> Bool -> ConwayCube
setXYZ cube (x,y,z) val = cube { slices = slices' }
  where slice   = slices cube !! z
        row     = slice !! y
        row'    = take x row ++ [val] ++ drop (x+1) row
        slice'  = take y slice ++ [row'] ++ drop (y+1) slice
        slices' = take z (slices cube) ++ [slice'] ++ drop (z+1) (slices cube)

getNeighbours :: ConwayCube -> Coord -> [Bool]
getNeighbours cube (x,y,z)  = map (getXYZ cube) ncoords
-- getNeighbours :: ConwayCube -> (Int, Int, Int) -> [(Int,Int,Int)]
-- getNeighbours cube (x,y,z)  = ncoords
  where offsets             = [-1,0,1]
        validc (x',y',z')   = (x',y',z') /= (x,y,z) && x' >= 0 && y' >= 0 && z' >= 0 && x' < w && y' < h && z' < d
        w                   = width cube
        h                   = height cube
        c                   = cycles cube
        d                   = depth cube
        ncoords             = filter validc [(x+x',y+y',z+z') | x' <- offsets, y' <- offsets, z' <- offsets]

-- (Read only cube, writable cube)
updateCell :: (ConwayCube, ConwayCube) -> Coord -> (ConwayCube, ConwayCube)
updateCell (rc, wc) coord
  | isalive   = if ncount == 2 || ncount == 3 then (rc, live) else (rc, die)
  | otherwise = if ncount == 3 then (rc, live) else (rc, die)
  where isalive     = getXYZ rc coord
        neighbours  = getNeighbours rc coord
        ncount      = length $ filter id neighbours
        die         = setXYZ wc coord False
        live        = setXYZ wc coord True

step :: ConwayCube -> ConwayCube
step cube = cube' { current = curr' }
  where coords        = [(x,y,z) | x <- [0..width cube - 1], y <- [0..height cube - 1], z <- [0..depth cube - 1]]
        cube'         = snd $ foldl updateCell (cube, cube) coords
        curr'         = current cube + 1

run :: ConwayCube -> ConwayCube
run cube
  | current cube < cycles cube  = run $ step cube
  | otherwise                   = cube

countActive :: ConwayCube -> Int
countActive cube = length $ filter id $ concat $ concat $ slices cube

part1 :: [String] -> Int
part1 = countActive . run . newConwayCube 6

main :: IO ()
main = do
  example <- lines <$> readFile "example17.txt"
  input   <- lines <$> readFile "input17.txt"

  testAndRun_ part1 [(example,112)] input
