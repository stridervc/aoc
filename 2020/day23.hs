import qualified Data.CircularList as C

import Data.Char (digitToInt, intToDigit)

type Cups = C.CList Int

parse :: String -> Cups
parse input = C.fromList $ fmap digitToInt input

destination :: Cups -> Int
destination cups = next active
  where Just active = C.focus cups
        list        = C.toList cups
        min         = minimum list
        max         = maximum list
        dec a       = if (a-1) < min then max else a-1
        next a      = if dec a `elem` list then dec a else next (dec a)

move :: Cups -> Cups
move cups = done
  where three           = tail $ take 4 $ C.rightElements cups
        Just active     = C.focus cups
        Just nextactive = C.focus $ C.rotR removed'
        removed'        = C.rotL $ C.removeR $ C.removeR $ C.removeR $ C.rotR cups
        Just removed    = C.rotateTo (destination removed') removed'
        inserted        = foldr C.insertL removed $ reverse three
        Just done       = C.rotateTo nextactive inserted

moveN :: Int -> Cups -> Cups
moveN n cups
  | n == 0    = cups
  | otherwise = moveN (n-1) $ move cups

after1 :: Cups -> String
after1 cups = map intToDigit $ init $ C.toList $ C.rotR on1
  where Just on1  = C.rotateTo 1 cups

main :: IO ()
main = do
  print $ after1 $ moveN 100 $ parse "247819356"
