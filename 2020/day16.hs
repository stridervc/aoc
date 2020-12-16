import AoC

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Data.List (nub, sort, transpose)
import Data.Maybe (isJust, catMaybes)

import qualified Data.Map.Strict as M

type RuleLimit  = (Int, Int)
type Rule       = (String, [RuleLimit])
type Ticket     = [Int]

parseRuleLimit :: Parser RuleLimit
parseRuleLimit = do
  minR <- P.many1 P.digit
  _ <- P.char '-'
  maxR <- P.many1 P.digit
  return (read minR, read maxR)

parseRuleLine :: Parser Rule
parseRuleLine = do
  name <- P.manyTill P.anyChar (P.try (P.string ": "))
  rule1 <- parseRuleLimit
  _ <- P.string " or "
  rule2 <- parseRuleLimit
  _ <- P.newline
  return (name, [rule1, rule2])

parseTicket :: Parser Ticket
parseTicket = do
  nums <- P.sepBy1 (P.many1 P.digit) (P.char ',')
  _ <- P.newline
  return $ map read nums

parseInput :: Parser ([Rule], Ticket, [Ticket])
parseInput = do
  rules <- P.manyTill parseRuleLine P.newline
  _ <- P.string "your ticket:\n"
  myticket <- parseTicket
  _ <- P.newline
  _ <- P.string "nearby tickets:\n"
  tickets <- P.many1 parseTicket

  return (rules, myticket, tickets)

expandRuleLimit :: RuleLimit -> [Int]
expandRuleLimit (s,l) = [s..l]

expandRules :: [Rule] -> [Int]
expandRules rules = sort $ nub $ concatMap expandRuleLimit limits
  where limits  = concatMap snd rules

-- get list of invalid numbers on ticket
invalidNums :: [Rule] -> Ticket -> [Int]
invalidNums rules ticket = [n | n <- ticket, n `notElem` rules']
  where rules'  = expandRules rules

type Input = ([Rule], Ticket, [Ticket])

part1 :: Input -> Int
part1 (rules, ticket, tickets) = sum $ concatMap (invalidNums rules) tickets

-- true iff ticket is valid
isValid :: [Rule] -> Ticket -> Bool
isValid rules ticket = null $ invalidNums rules ticket

-- true iff number falls within rule's ranges
satisfiesRule :: Rule -> Int -> Bool
satisfiesRule rule num = num `elem` range1 || num `elem` range2
  where range1  = [fst r1range..snd r1range]
        r1range = head $ snd rule
        range2  = [fst r2range..snd r2range]
        r2range = snd rule !! 1

-- return list of rules that satisfies all these values
satisfiesAll :: [Rule] -> [Int] -> [Rule]
satisfiesAll rules nums = [r | r <- rules, all (satisfiesRule r) nums]

findOneRule :: [Int] -> [Rule] -> Maybe Rule
findOneRule nums rules
  | numsat == 1 = Just $ head satisfies
  | otherwise   = Nothing
  where satisfies = satisfiesAll rules nums
        numsat    = length satisfies

removeRule :: Rule -> [Rule] -> [Rule]
removeRule _ [] = []
removeRule r (x:xs)
  | r == x    = xs
  | otherwise = r : removeRule r xs

data Solver = Solver
  { nums    :: ![[Int]]
  , found   :: M.Map Int Rule
  , working :: ![Rule]
  , index   :: !Int
  , amount  :: !Int
  } deriving (Eq, Show)

newSolver :: [Ticket] -> [Rule] -> Solver
newSolver tickets rules = Solver
  { nums    = transpose tickets
  , found   = mempty
  , working = rules
  , index   = 0
  , amount  = length rules
  }

step :: Solver -> Solver
step solver
  | isJust  f = solver { index = newindex }
  | otherwise = solver { found = newfound, working = newworking, index = newindex }
  where i           = index solver
        f           = M.lookup i $ found solver
        newindex    = if i >= amount solver - 1 then 0 else i+1
        foundrule   = findOneRule (nums solver !! i) (working solver)
        newfound    = case foundrule of
                        Just rule -> M.insert i rule $ found solver
                        Nothing   -> found solver
        newworking  = case foundrule of
                        Just rule -> removeRule rule (working solver)
                        Nothing   -> working solver

solvedRules :: Solver -> [Rule]
solvedRules = M.elems . found

isSolved :: Solver -> Bool
isSolved = null . working

solve :: Solver -> Solver
solve solver
  | isSolved solver = solver
  | otherwise       = solve $ step solver

part2' :: Input -> Solver
part2' (rules, ticket, tickets) = solve $ newSolver tickets' rules
  where tickets'  = filter (isValid rules) tickets

part2 :: Input -> Int
part2 (rules, ticket, tickets) = sum relevant
  where tickets'    = filter (isValid rules) tickets
        solved      = solve $ newSolver tickets' rules
        solvedrules = solvedRules solved
        zipped      = zip solvedrules [0..]
        filtered    = filter (\(r,_) -> head (words (fst r)) == "departure") zipped
        relevant    = map ((ticket!!) . snd) filtered

main :: IO ()
main = do
  example1Input <- readFile "example16.txt"
  example2Input <- readFile "example16b.txt"
  actualInput  <- readFile "input16.txt"

  let Right example1  = P.parse parseInput "(example16)"  example1Input
  let Right example2  = P.parse parseInput "(example16b)" example2Input
  let Right actual    = P.parse parseInput "(actual16)"   actualInput

  {-
  putStrLn "Part 1"
  testAndRun_ part1 [(example1, 71)] actual
  -}

  putStrLn "Part 2"
  timer <- startTimer

  print $ part2 actual
  -- print $ part2' example2

  printTimerLn timer
