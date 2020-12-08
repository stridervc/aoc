import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import qualified Data.Map as M

data Operation    = Nop | Acc | Jmp deriving (Eq, Show)
data MachineExit  = Busy | Duplicate | Terminate deriving (Eq, Show)

type Instruction = (Operation,Int)

data MachineState = MachineState
  { acc     :: Int
  , ip      :: Int
  , mem     :: [Instruction]
  , visited :: M.Map Int Bool   -- map of which memory locations have been executed
  } deriving (Eq, Show)

opParser :: Parser Operation
opParser =
  Parsec.choice
    [ Parsec.string "nop" >> return Nop
    , Parsec.string "acc" >> return Acc
    , Parsec.string "jmp" >> return Jmp
    ]

argParser :: Parser Int
argParser = do
  sign <- Parsec.char '+' <|> Parsec.char '-'
  num <- Parsec.many1 Parsec.digit
  case sign of
    '-' -> return $ (-1) * (read num)
    _   -> return $ read num

instrParser :: Parser Instruction
instrParser = do
  op <- opParser
  Parsec.space
  arg <- argParser
  return (op,arg)

-- create a blank machine state
newMachine :: MachineState
newMachine = MachineState
  { acc     = 0
  , ip      = 0
  , mem     = []
  , visited = M.empty
  }

-- load machine memory from list of instructions
loadMachine :: MachineState -> [Instruction] -> MachineState
loadMachine m instrs = m { mem = instrs }

-- convenience boolean lookup from map
lookupBool k m = case M.lookup k m of
  Nothing -> False
  Just v  -> v

-- execute arbitrary instruction on machine (not from memory),
-- registers (ip,acc) are updated
execInstr :: MachineState -> Instruction -> MachineState
execInstr m (op,arg)
  | op  == Nop  = m { ip = ip' + 1 }
  | op  == Acc  = m { ip = ip' + 1, acc = acc' + arg }
  | op  == Jmp  = m { ip = ip' + arg }
  where ip'       = ip m
        acc'      = acc m

-- execute one instruction from memory on machine
-- return exit status and new machine state
step :: MachineState -> (MachineExit, MachineState)
step m
  | duplicate = (Duplicate, m)
  | ip' >= n  = (Terminate, m)
  | otherwise = (Busy, m')
  where duplicate = lookupBool ip' visited'
        n         = length $ mem m
        ip'       = ip m
        visited'  = visited m
        instr'    = mem m !! ip'
        m''       = m { visited = M.insert ip' True visited' }
        m'        = execInstr m'' instr'

-- run a machine until it's about to execute an instruction for
-- the second time, or terminates normally
run :: MachineState -> (MachineExit, MachineState)
run m
  | exit == Busy  = run m'
  | otherwise     = (exit,m)
  where (exit,m') = step m

flipNopJmp :: Instruction -> Instruction
flipNopJmp (op,arg)
  | op  == Nop  = (Jmp,arg)
  | op  == Jmp  = (Nop,arg)
  | otherwise   = (op, arg)

-- flip instr at pos from jmp to nop or nop to jmp
flipPos :: MachineState -> Int -> MachineState
flipPos m i = m { mem = flipped }
  where mem'    = mem m
        instr'  = mem' !! i
        flipped = take i mem' ++ [flipNopJmp instr'] ++ drop (i+1) mem'

-- fix machine by changing a nop to a jmp, or a jmp to a nop
-- return first machine state that terminates normally
fix :: MachineState -> (MachineExit, MachineState)
fix m = head $ dropWhile (\em -> fst em /= Terminate) $ map (run . flipPos m) [0..]

main = do
  contents <- lines <$> readFile "input8.txt"

  let Right instrs = traverse (Parsec.parse instrParser "(input)") contents
  let m = loadMachine newMachine instrs

  print $ (acc . snd) $ run m
  print $ (acc . snd) $ fix m
