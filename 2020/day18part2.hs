import AoC

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import Data.Either (rights)

-- try to find a multiply, which is lowest priority, then multiply left with right
-- each of which is parsed as well, on the left, go up one priority (since we skipped
-- all multiplications there anyway), but on the right, we could still have multiplication
-- if there aren't any multiplications, move on to next priority parser
pri0Parser :: Parser Int
pri0Parser =
  P.try (do
    p1 <- pri1Parser
    P.spaces
    P.char '*'
    P.spaces
    p0 <- pri0Parser
    return $ p1 * p0
    )
  <|> pri1Parser

-- addition
pri1Parser :: Parser Int
pri1Parser =
  P.try (do
    p2 <- pri2Parser
    P.spaces
    P.char '+'
    P.spaces
    p1 <- pri1Parser
    return $ p2 + p1
    )
  <|> pri2Parser

-- brackets, or actual numbers
pri2Parser :: Parser Int
pri2Parser =
  P.try (do
    P.char '('
    P.spaces
    p0 <- pri0Parser
    P.spaces
    P.char ')'
    P.spaces
    return p0
    )
  <|> (do
    num <- P.many1 P.digit
    return $ read num
    )

part2 :: [String] -> Int
part2 input = sum $ rights nums
  where nums = map (P.parse pri0Parser "") input

main :: IO ()
main = do
  let example1 = "2 * 3 + (4 * 5)"
  let example2 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
  let example3 = "8 * 3 + 9 + 3 * 4 * 3"

  input <- lines <$> readFile "input18.txt"

  testAndRun_ part2
    [ ([example1], 46)
    , ([example2], 1445)
    , ([example3], 1440)
    ] input
