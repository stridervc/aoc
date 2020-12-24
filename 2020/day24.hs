import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import qualified Data.Map as M

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast
  deriving (Eq, Show)

type Coord = (Int, Int)

-- False = White, True = Black
type HexGrid = M.Map Coord Bool

parseDirection :: Parser Direction
parseDirection = do
  P.try (P.string "sw" >> return SouthWest)
    <|> P.try (P.string "se" >> return SouthEast)
    <|> P.try (P.string "nw" >> return NorthWest)
    <|> P.try (P.string "ne" >> return NorthEast)
    <|> (P.char 'e' >> return East)
    <|> (P.char 'w' >> return West)

parseLine :: Parser [Direction]
parseLine = P.many parseDirection

move :: Direction -> Coord -> Coord
move East       (x,y) = (x+2, y)
move SouthEast  (x,y) = (x+1, y+1)
move SouthWest  (x,y) = (x-1, y+1)
move West       (x,y) = (x-2, y)
move NorthWest  (x,y) = (x-1, y-1)
move NorthEast  (x,y) = (x+1, y-1)

applyDirections :: [Direction] -> HexGrid -> HexGrid
applyDirections ds h = M.insert coord (not curr) h
  where coord = foldl (flip move) (0,0) ds
        curr  = M.findWithDefault False coord h

part1 :: String -> Int
part1 input = M.size $ M.filter id hexgrid
  where Right directions  = mapM (P.parse parseLine "input") $ lines input
        hexgrid           = foldl (flip applyDirections) M.empty directions

main :: IO ()
main = do
  example <- readFile "example24.txt"
  input   <- readFile "input24.txt"

  print $ part1 example
  print $ part1 input
