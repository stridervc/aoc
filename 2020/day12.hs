type Instruction  = String

data Ship = Ship
  { posx      :: Int
  , posy      :: Int
  , direction :: Int
  } deriving (Eq, Show)

newShip :: Ship
newShip =
  Ship  { posx  = 0
        , posy  = 0
        , direction = 90
        }

applyInstruction :: Ship -> Instruction -> Ship
applyInstruction ship instruction
  | instr == 'N'  = ship { posy = y - arg }
  | instr == 'S'  = ship { posy = y + arg }
  | instr == 'E'  = ship { posx = x + arg }
  | instr == 'W'  = ship { posx = x - arg }
  | instr == 'L'  = ship { direction  = (dir - arg) `mod` 360 }
  | instr == 'R'  = ship { direction  = (dir + arg) `mod` 360 }
  | instr == 'F'  = case dir of
                      0   -> ship { posy = y - arg }
                      90  -> ship { posx = x + arg }
                      180 -> ship { posy = y + arg }
                      270 -> ship { posx = x - arg }
  where instr = head instruction
        arg   = read $ tail instruction
        x     = posx ship
        y     = posy ship
        dir   = direction ship

apply :: Ship -> [Instruction] -> Ship
apply = foldl applyInstruction

distance :: Ship -> Ship -> Int
distance a b = abs (posx a) - abs (posx b) + abs (posy a) - abs (posy b)

main :: IO ()
main = do
  instructions <- lines <$> readFile "input12.txt"
  let ship = apply newShip instructions
  print $ distance ship newShip
