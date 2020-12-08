import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import qualified Data.Map as M

data Operation = Nop | Acc | Jmp deriving (Eq, Show)

type Instruction = (Operation,Int)

data MachineState = MachineState
  { acc     :: Int
  , ip      :: Int
  , mem     :: [Instruction]
  , visited :: M.Map Int Bool
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
loadMachine m is = m { mem = is }

-- convenience boolean lookup
lookupBool k m = case M.lookup k m of
  Nothing -> False
  Just v  -> v

-- execute instruction on machine
execInstr :: MachineState -> Instruction -> MachineState
execInstr m (op,arg)
  | op  == Nop  = m { ip = ip' + 1 }
  | op  == Acc  = m { ip = ip' + 1, acc = acc' + arg }
  | op  == Jmp  = m { ip = ip' + arg }
  where ip'       = ip m
        acc'      = acc m

-- execute one instruction on machine, unless it has already
-- been executed, in which case, return false
step :: MachineState -> (Bool, MachineState)
step m
  | duplicate = (False, m)
  | otherwise = (True, m')
  where duplicate = lookupBool ip' visited'
        ip'       = ip m
        visited'  = visited m
        instr'    = mem m !! ip'
        m''       = m { visited = M.insert ip' True visited' }
        m'        = execInstr m'' instr'

-- run a machine until it's about to execute an instruction for
-- the 2nd time, return with last machine state
run :: MachineState -> MachineState
run m
  | not success = m
  | otherwise   = run m'
  where (success,m')  = step m

main = do
  contents <- lines <$> readFile "input8.txt"

  let Right instrs = traverse (Parsec.parse instrParser "(input)") contents

  print $ acc $ run $ loadMachine newMachine instrs
