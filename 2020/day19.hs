import AoC

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import qualified Data.Map as M

data Rule = RuleSeq [Rule] | RuleOption [Rule] [Rule] | RuleChar Char
  deriving (Eq, Show)

type RuleStr  = String
type Message  = String

parseRuleLine :: Parser (Int, RuleStr)
parseRuleLine = do
  index <- P.many1 P.digit
  P.string ": "
  rulestr <- P.many1 (P.noneOf "\n")
  P.char '\n'
  return (read index, rulestr)

parseMessage :: Parser Message
parseMessage = do
  msg <- P.many1 (P.char 'a' <|> P.char 'b')
  P.char '\n'
  return msg

parseInput :: Parser ([(Int, RuleStr)], [Message])
parseInput = do
  rules <- P.many1 parseRuleLine
  P.char '\n'
  msgs <- P.many1 parseMessage
  P.eof
  return (rules, msgs)

main :: IO ()
main = do
  example <- readFile "example19.txt"

  print $ P.parse parseInput "(example19)" example
