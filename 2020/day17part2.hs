import AoC

import Control.Monad (when)
import Text.Printf (printf)

type ZSlice   = [[Bool]]
type WSlice   = [ZSlice]
type Coord4D  = (Int, Int, Int, Int)

data ConwayTesseract = ConwayTesseract
  { cycles  :: !Int      -- the number of cycles this cube will support, determines max size
  , current :: !Int      -- current cycle
  , slices  :: ![WSlice]
  , width   :: !Int
  , height  :: !Int
  , depth   :: !Int
  , extra   :: !Int      -- the 4th spatial dimension's size
  } deriving (Eq, Show)

newConwayTesseract :: Int -> [String] -> ConwayTesseract
newConwayTesseract cycles input = ConwayTesseract
  { cycles  = cycles
  , current = 0
  , slices  = replicate cycles blankzslice ++ [zslice] ++ replicate cycles blankzslice
  , width   = w
  , height  = h
  , depth   = d
  , extra   = e
  }
  where blankslice  = replicate h $ replicate w False
        iw          = length $ head input
        ih          = length input
        w           = iw + 2*cycles
        h           = ih + 2*cycles
        d           = 1  + 2*cycles
        e           = 1  + 2*cycles
        zslice      = replicate cycles blankslice ++ [startslice] ++ replicate cycles blankslice
        blankzslice = replicate (2*cycles+1) blankslice
        tempslice   = map (map (== '#')) input
        padrows     = replicate cycles $ replicate w False
        padslice    = map (\r -> replicate cycles False ++ r ++ replicate cycles False) tempslice
        startslice  = padrows ++ padslice ++ padrows

printConwayTesseract :: ConwayTesseract -> IO ()
printConwayTesseract cube = do
  printf "Cycle %d / %d\n" (current cube) (cycles cube)
  printf "Width   = %d (%d)\n" (width cube) (length $ head $ head $ head $ slices cube)
  printf "Height  = %d (%d)\n" (height cube) (length $ head $ head $ slices cube)
  printf "Depth   = %d (%d)\n" (depth cube) (length $ head $ slices cube)
  printf "Extra   = %d (%d)\n" (extra cube) (length $ slices cube)

  {-
  -- printf "Slices = %d\n\n" (length $ slices cube)
  mapM_ printSlice $ zip [0..] $ slices cube
  where printSlice (z, slice)  = when (or $ concat slice) (do
                                  putStr "z = "
                                  print $ z - cycles cube
                                  mapM_ (putStrLn . map (\c -> if c then '#' else '.')) slice
                                  )
                                  -}

getXYZW :: ConwayTesseract -> Coord4D -> Bool
getXYZW cube (x,y,z,w) = slices cube !! w !! z !! y !! x

setXYZW :: ConwayTesseract -> Coord4D -> Bool -> ConwayTesseract
setXYZW cube (x,y,z,w) val  = cube { slices = wslices' }
  where wslice    = slices cube !! w
        slice     = wslice !! z
        row       = slice !! y
        row'      = take x row ++ [val] ++ drop (x+1) row
        slice'    = take y slice ++ [row'] ++ drop (y+1) slice
        slices'   = take z wslice ++ [slice'] ++ drop (z+1) wslice
        wslices'  = take w (slices cube) ++ [slices'] ++ drop (w+1) (slices cube)

getNeighbours :: ConwayTesseract -> Coord4D -> [Bool]
getNeighbours cube (x,y,z,w)  = map (getXYZW cube) ncoords
  where offsets               = [-1,0,1]
        validc (x',y',z',w')  = (x',y',z',w') /= (x,y,z,w) && x' >= 0 && y' >= 0 && z' >= 0 && w' >= 0 && x' < width' && y' < h && z' < d && w' < e
        width'                = width cube
        h                     = height cube
        d                     = depth cube
        e                     = extra cube
        ncoords               = filter validc [(x+x',y+y',z+z',w+w') | x' <- offsets, y' <- offsets, z' <- offsets, w' <- offsets]

-- (Read only cube, writable cube)
updateCell :: (ConwayTesseract, ConwayTesseract) -> Coord4D -> (ConwayTesseract, ConwayTesseract)
updateCell (rc, wc) coord
  | isalive   = if ncount == 2 || ncount == 3 then (rc, live) else (rc, die)
  | otherwise = if ncount == 3 then (rc, live) else (rc, die)
  where isalive     = getXYZW rc coord
        neighbours  = getNeighbours rc coord
        ncount      = length $ filter id neighbours
        die         = setXYZW wc coord False
        live        = setXYZW wc coord True

step :: ConwayTesseract -> ConwayTesseract
step cube = cube' { current = curr' }
  where coords  = [(x,y,z,w) | x <- [0..width cube - 1], y <- [0..height cube - 1], z <- [0..depth cube - 1], w <- [0..extra cube - 1]]
        cube'   = snd $! foldl updateCell (cube, cube) coords
        curr'   = current cube + 1

run :: ConwayTesseract -> ConwayTesseract
run cube
  | current cube < cycles cube  = run $! step cube
  | otherwise                   = cube

countActive4D :: ConwayTesseract -> Int
countActive4D cube = length $ filter id $ concat $ concat $ concat $ slices cube

part2 :: [String] -> Int
part2 = countActive4D . run . newConwayTesseract 6

main :: IO ()
main = do
  example <- lines <$> readFile "example17.txt"
  input   <- lines <$> readFile "input17.txt"

  testAndRun_ part2 [(example,848)] input
