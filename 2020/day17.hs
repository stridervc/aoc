import qualified Data.Vector.Unboxed as V

import Control.Monad (when)

type ZSlice = V.Vector Bool

data ConwayCube = ConwayCube
  { cycles  :: Int      -- the number of cycles this cube will support, determines max size
  , slices  :: [ZSlice]
  } deriving (Eq, Show)

newConwayCube :: Int -> [String] -> ConwayCube
newConwayCube cycles input = ConwayCube
  { cycles  = cycles
  , slices  = replicate cycles blankslice ++ [startslice] ++ replicate cycles blankslice
  }
  where blankslice  = V.fromList $ replicate ((w+2*cycles) * (h+2*cycles)) False
        h           = length input
        w           = length $ head input
        startslice  = V.fromList $ map (== '#') $ concat input

printConwayCube :: ConwayCube -> IO ()
printConwayCube cube = do
  putStrLn $ "Cycles: " ++ show (cycles cube)
  putStrLn ""
  mapM_ printSlice $ slices cube
  where printSlice slice  = when (or $ V.toList slice) (print slice)

main :: IO ()
main = do
  input <- lines <$> readFile "example17.txt"

  printConwayCube $ newConwayCube 6 input
