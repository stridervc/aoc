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

-- to 36bit binary as string
toBinary :: Integer -> String
toBinary num = replicate pad '0' ++ bin
  where bin = reverse $ unfoldr (\b -> if b == 0 then Nothing else Just (if b `mod` 2 == 0 then '0' else '1', b `div` 2)) num
        pad = 36 - length bin

-- string representation of binary number to number
toDecimal :: String -> Integer
toDecimal s = foldr (\i acc -> if digit i == '1' then acc + 2^i else acc) 0 [0..length s - 1]
  where digit i = reverse s !! i

-- part 1
applyMask1 :: Mask -> Integer -> Integer
applyMask1 mask num = toDecimal $ map (\i -> if mask!!i == 'X' then bin!!i else mask!!i) [0..length mask - 1]
  where bin = toBinary num

-- for the masked number, and the current number
-- give the next number in sequence
nextMasked :: Mask -> Mask -> Mask
nextMasked []       []        = []
nextMasked ('X':ms) ('0':ns)  = '1' : ns
nextMasked ('X':ms) ('1':ns)  = '0' : nextMasked ms ns
nextMasked (_  :ms) (n  :ns)  = n   : nextMasked ms ns

allUnrolled :: Mask -> [Mask]
allUnrolled masked = start : unfoldr (\b -> if b == zeroed then Nothing else Just (nextm b, nextm b)) start
  where zeroed  = map (\c -> if c == 'X' then '0' else c) masked
        nextm   = nextMasked masked
        start   = nextm zeroed

-- part 2
applyMask2 :: Mask -> Integer -> [Integer]
applyMask2 mask num = map toDecimal $ allUnrolled masked
  where bin     = toBinary num
        masked  = map (\i -> if mask!!i == '0' then bin!!i else mask!!i) [0..length mask - 1]

-- part 1
applyInstr1 :: MachineState -> Instruction -> MachineState
applyInstr1 m (SetMask mask) = m { mask = mask }
applyInstr1 m (SetMem (i,v)) = m { mem  = M.insert i v' $ mem m }
  where v'  = applyMask1 (mask m) v

-- part 2
applyInstr2 :: MachineState -> Instruction -> MachineState
applyInstr2 m (SetMask mask) = m { mask = mask }
applyInstr2 m (SetMem (i,v)) = m { mem  = mem' }
  where is    = applyMask2 (mask m) i
        mem'  = foldl (\m i -> M.insert i v m) (mem m) is

-- solve using given applyInstr function
solve applyInstr is = sum values
  where machine = foldl applyInstr newMachine is
        memory  = mem machine
        keys    = M.keys memory
        values  = mapMaybe (`M.lookup` memory) keys

part1 = solve applyInstr1
part2 = solve applyInstr2

main :: IO ()
main = do
  example1data <- lines <$> readFile "example14a.txt"
  example2data <- lines <$> readFile "example14b.txt"
  actualdata   <- lines <$> readFile "input14.txt"

  let Right example1 = traverse (P.parse instrParser "(example1)") example1data
  let Right example2 = traverse (P.parse instrParser "(example2)") example2data
  let Right actual   = traverse (P.parse instrParser "(actual)")  actualdata

  putStrLn "Part 1"
  testAndRun_ part1 [(example1, 165)] actual

  putStrLn ""
  putStrLn "Part 2"
  testAndRun_ part2 [(example2, 208)] actual
