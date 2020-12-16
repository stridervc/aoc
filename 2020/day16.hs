import AoC

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Data.List (nub, sort)

type Rule   = (Int, Int)
type Ticket = [Int]

parseRule :: Parser Rule
parseRule = do
  minR <- P.many1 P.digit
  _ <- P.char '-'
  maxR <- P.many1 P.digit
  return (read minR, read maxR)

parseRuleLine :: Parser [Rule]
parseRuleLine = do
  _ <- P.manyTill P.anyChar (P.try (P.string ": "))
  rule1 <- parseRule
  _ <- P.string " or "
  rule2 <- parseRule
  _ <- P.newline
  return [rule1, rule2]

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

  return (concat rules, myticket, tickets)

expandRule :: Rule -> [Int]
expandRule (s,l) = [s..l]

expandRules :: [Rule] -> [Int]
expandRules rules = sort $ nub $ concatMap expandRule rules

invalidNums :: [Rule] -> Ticket -> [Int]
invalidNums rules ticket = [n | n <- ticket, n `notElem` rules']
  where rules'  = expandRules rules

type Input = ([Rule], Ticket, [Ticket])

part1 :: Input -> Int
part1 (rules, ticket, tickets) = sum $ concatMap (invalidNums rules) tickets

main :: IO ()
main = do
  exampleInput <- readFile "example16.txt"
  actualInput  <- readFile "input16.txt"

  let Right example = P.parse parseInput "(example16)" exampleInput
  let Right actual  = P.parse parseInput "(actual16)"  actualInput

  testAndRun_ part1 [(example, 71)] actual
