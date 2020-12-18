import AoC

import Data.Char (isDigit, digitToInt)
import Data.Maybe (isJust)

data Calc = Calc
  { number    :: Int
  , operator  :: Maybe Char
  } deriving (Eq, Show)

newCalc :: Calc
newCalc = Calc
  { number    = 0
  , operator  = Nothing
  }

inputNum :: Int -> Calc -> Calc
inputNum num calc
  | hasop     = case op of
                  '+' -> calc' { number = n' + num }
                  _   -> calc' { number = n' * num }
  | otherwise = calc { number = num }
  where hasop   = isJust $ operator calc
        calc'   = calc { operator = Nothing }
        Just op = operator calc
        n'      = number calc

process :: (String, Calc) -> (String, Calc)
process ([], calc)      = ([], calc)
process (' ':cs, calc)  = process (cs, calc)
process ('(':cs, calc)  = let (cs', calc') = process (cs, newCalc) in process (cs', inputNum (number calc') calc)
process (')':cs, calc)  = (cs, calc)
process (c:cs, calc)
  | digit               = process (cs, inputNum (digitToInt c) calc)
  | otherwise           = process (cs, calc { operator = Just c })
  where digit     = isDigit c

part1 :: [String] -> Int
part1 input = sum $ map (number . snd . (\s -> process (s, newCalc))) input

main :: IO ()
main = do
  let example1 = "2 * 3 + (4 * 5)"
  let example2 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"

  input <- lines <$> readFile "input18.txt"

  testAndRun_ part1 [([example1], 26), ([example2], 437)] input
