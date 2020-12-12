type Instruction  = String

data Ship = Ship
  { posx  :: Int
  , posy  :: Int
  , wayx  :: Int
  , wayy  :: Int
  } deriving (Eq, Show)

-- ship in starting position
newShip :: Ship
newShip =
  Ship  { posx  = 0
        , posy  = 0
        , wayx  = 10
        , wayy  = -1
        }

-- rotate waypoint counter clockwise by 90 deg a number of times
rotateCCW :: Ship -> Int -> Ship
rotateCCW ship 1 = ship { wayx = wayy ship, wayy = - wayx ship }
rotateCCW ship c = rotateCCW (rotateCCW ship 1) (c-1)

-- rotate waypoint clockwise by 90 deg a number of times
rotateCW :: Ship -> Int -> Ship
rotateCW ship 1 = rotateCCW ship 3
rotateCW ship c = rotateCW (rotateCW ship 1) (c-1)

-- apply a single instruction to ship
applyInstruction :: Ship -> Instruction -> Ship
applyInstruction ship instruction
  | instr == 'N'  = ship { wayy = wy - arg }
  | instr == 'S'  = ship { wayy = wy + arg }
  | instr == 'E'  = ship { wayx = wx + arg }
  | instr == 'W'  = ship { wayx = wx - arg }
  | instr == 'L'  = rotateCCW ship (arg `div` 90)
  | instr == 'R'  = rotateCW ship (arg `div` 90)
  | instr == 'F'  = ship { posx = x + arg * wx, posy = y + arg * wy }
  where instr = head instruction
        arg   = read $ tail instruction
        x     = posx ship
        y     = posy ship
        wx    = wayx ship
        wy    = wayy ship

-- apply list of instructions to ship
apply :: Ship -> [Instruction] -> Ship
apply = foldl applyInstruction

-- calculate manhattan distance between two ships
distance :: Ship -> Ship -> Int
distance a b = abs (posx a) - abs (posx b) + abs (posy a) - abs (posy b)

main :: IO ()
main = do
  instructions <- lines <$> readFile "input12.txt"
  let ship = apply newShip instructions
  print $ distance ship newShip
