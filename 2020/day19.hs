import AoC

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import qualified Data.Map as M

data Rule = RuleSeq [Rule] | RuleOption [Rule] [Rule] | RuleChar Char
  deriving (Eq, Show)

type RuleStr  = String
type Message  = String

type  RulesMap = M.Map Int String

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

numParser :: Parser Int
numParser = do
  num <- P.many1 P.digit
  P.spaces
  return $ read num

ruleParser :: RulesMap -> Parser Rule
ruleParser rules =
  P.try (do
    P.char '"'
    ch <- P.letter
    P.char '"'
    return $ RuleChar ch
    )
  <|> P.try (do
    numsl <- P.many1 numParser
    P.string "| "
    numsr <- P.many1 numParser
    return $ RuleOption (map rl numsl) (map rl numsr)
    )
  <|> (do
    nums <- P.sepBy1 (P.many1 P.digit) P.space
    return $ RuleSeq $ map (rl . read) nums
    )
  where rl = parsedRule rules

parsedRule :: RulesMap -> Int -> Rule
parsedRule rules i  = case P.parse (ruleParser rules) ("(ruleParser rule " ++ show i ++ ")") rule of
                        Right rule  -> rule
                        Left err    -> error $ show err
  where Just rule = M.lookup i rules

satisfiesRules :: [Rule] -> String -> (Bool, String)
satisfiesRules rules                    []      = (null rules, [])
satisfiesRules []                       cs      = (True, cs)
satisfiesRules (RuleChar ch:rs)         (c:cs)  = let (ret, cs') = satisfiesRules rs cs in (c == ch && ret, cs')
satisfiesRules (RuleOption rs1 rs2:rs)  cs
  | left      = satisfiesRules rs csl
  | right     = satisfiesRules rs csr
  | otherwise = (False, cs)
  where (left, csl)   = satisfiesRules rs1 cs
        (right, csr)  = satisfiesRules rs2 cs

satisfiesRules (RuleSeq rules:rs)       cs      = (retseq && ret, cs')
  where (retseq, csseq) = satisfiesRules rules cs
        (ret, cs')      = satisfiesRules rs csseq

satisfiesRule :: Rule -> String -> Bool
satisfiesRule rule cs = ret && null cs'
  where (ret, cs')  = satisfiesRules [rule] cs

part1 :: String -> Int
part1 input = length $ filter id $ map (satisfiesRule (parsedRule rules 0)) msgs
  where Right (rulestrs, msgs)  = P.parse parseInput "" input
        rules                   = M.fromList rulestrs

main :: IO ()
main = do
  example <- readFile "example19.txt"
  actual  <- readFile "input19.txt"

  testAndRun_ part1 [(example, 2)] actual

