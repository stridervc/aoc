import Control.Monad (when)
import Data.List.Split (chunksOf)
import Text.Printf (printf)

type ZSlice = [Bool]

data ConwayCube = ConwayCube
  { cycles  :: !Int      -- the number of cycles this cube will support, determines max size
  , current :: !Int      -- Current cycle
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
  where blankslice  = replicate (w*h) False
        iw          = length $ head input
        ih          = length input
        w           = iw + 2*cycles
        h           = ih + 2*cycles
        d           = 1  + 2*cycles
        -- TODO startslice needs padding on all 4 sides
        startslice  = map (== '#') $ concat input

-- just for debugging
printConwayCube :: ConwayCube -> IO ()
printConwayCube cube = do
  printf "Cycle %d / %d\n" (current cube) (cycles cube)
  printf "Slices = %d\n\n" (length $ slices cube)
  mapM_ printSlice $ zip [0..] $ slices cube
  where printSlice (z, slice)  = when (or slice) (do
                                  putStr "z = "
                                  print $ z - cycles cube
                                  mapM_ putStrLn $ chunksOf (width cube) $ map (\c -> if c then '#' else '.') slice
                                  )

-- we can work with (0,0,0) being the top left of the slice we give newConwayCube
-- this gives the coords as ConwayCube uses them
{-
actualCoords :: ConwayCube -> (Int, Int, Int) -> (Int, Int, Int)
actualCoords cube (x,y,z) = (x+c, y+c, z+c)
  where c = cycles cube
-}

getXYZ :: ConwayCube -> (Int, Int, Int) -> Bool
getXYZ cube (x,y,z) = slice !! (y*w+x)
  where slice   = slices cube !! z
        w       = width cube

setXYZ :: ConwayCube -> (Int, Int, Int) -> Bool -> ConwayCube
setXYZ cube (x,y,z) val = cube { slices = slices' }
  where slice   = slices cube !! z
        slice'  = take index slice ++ [val] ++ drop (index+1) slice
        index   = y * width cube + x
        slices' = take z (slices cube) ++ [slice'] ++ drop (z-1) (slices cube)

getNeighbours :: ConwayCube -> (Int, Int, Int) -> [Bool]
getNeighbours cube (x,y,z)  = map (getXYZ cube) ncoords
-- getNeighbours :: ConwayCube -> (Int, Int, Int) -> [(Int,Int,Int)]
-- getNeighbours cube (x,y,z)  = ncoords
  where offsets             = [-1,0,1]
        validc (x',y',z')   = (x',y',z') /= (x,y,z) && x' >= 0 && y' >= 0 && z' >= 0 && x' < w+c && y' < h+c && z' < d
        w                   = width cube
        h                   = height cube
        c                   = cycles cube
        d                   = depth cube
        ncoords             = filter validc [(x+x',y+y',z+z') | x' <- offsets, y' <- offsets, z' <- offsets]

step :: ConwayCube -> ConwayCube
step cube = undefined
  where coords  = [(x,y,z) | x <- [0..width cube - 1], y <- [0..height cube - 1], z <- [0..length (slices cube) -1]]

main :: IO ()
main = do
  input <- lines <$> readFile "example17.txt"

  printConwayCube $ newConwayCube 6 input

  print $ getNeighbours (newConwayCube 6 input) (6,6,6)
