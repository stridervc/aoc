import AoC

import qualified Data.Map as M

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import Data.List (unfoldr)
import Data.Maybe (mapMaybe)

type Mask = String

data Instruction = SetMask String | SetMem (Integer, Integer)
  deriving (Eq, Show)

data MachineState = MachineState
  { mask  :: Mask
  , mem   :: M.Map Integer Integer
  } deriving (Eq, Show)

newMachine :: MachineState
newMachine = MachineState
  { mask  = replicate 36 'X'
  , mem   = M.empty
  }

maskParser :: Parser Instruction
maskParser = do
  P.string "mask = "
  mask <- P.many1 P.anyChar
  return $ SetMask mask

memParser :: Parser Instruction
memParser = do
  P.string "mem["
  loc <- P.many1 P.digit
  P.string "] = "
  val <- P.many1 P.digit
  return $ SetMem (read loc, read val)

instrParser :: Parser Instruction
instrParser = P.try maskParser <|> memParser

toBinary :: Integer -> String
toBinary num = replicate pad '0' ++ bin
  where bin = reverse $ unfoldr (\b -> if b == 0 then Nothing else Just (if b `mod` 2 == 0 then '0' else '1', b `div` 2)) num
        pad = 36 - length bin

toDecimal :: String -> Integer
toDecimal s = foldr (\i acc -> if digit i == '1' then acc + 2^i else acc) 0 [0..length s - 1]
  where digit i = reverse s !! i

applyMask :: Mask -> Integer -> Integer
applyMask mask num = toDecimal $ map (\i -> if mask!!i == 'X' then bin!!i else mask!!i) [0..length mask - 1]
  where bin = toBinary num

applyInstr :: MachineState -> Instruction -> MachineState
applyInstr m (SetMask mask) = m { mask = mask }
applyInstr m (SetMem (i,v)) = m { mem  = M.insert i v' $ mem m }
  where v'  = applyMask (mask m) v

applyInstrs :: MachineState -> [Instruction] -> MachineState
applyInstrs = foldl applyInstr

part1 :: [Instruction] -> Integer
part1 is = sum values
  where machine = applyInstrs newMachine is
        memory  = mem machine
        keys    = M.keys memory
        values  = mapMaybe (`M.lookup` memory) keys

main :: IO ()
main = do
  exampledata <- lines <$> readFile "example14.txt"
  actualdata  <- lines <$> readFile "input14.txt"

  let Right example = traverse (P.parse instrParser "(example)") exampledata
  let Right actual  = traverse (P.parse instrParser "(actual)")  actualdata

  testAndRun_ part1 [(example, 165)] actual
