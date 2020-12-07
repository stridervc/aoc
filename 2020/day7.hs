{-# LANGUAGE FlexibleContexts #-}

import Data.List.Split (splitOn)
import Data.Either (rights)

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))

type Bag    = String
type Count  = Int
type Rule   = (Bag, [(Count,Bag)])
type Rules  = [Rule]

{- example rules
 - mirrored violet bags contain 1 bright beige bag.
 - dim olive bags contain no other bags.
 - striped crimson bags contain 1 pale orange bag, 5 dim white bags, 3 clear fuchsia bags.
 -}

-- convenience type
-- Parse a String, no state, and return type a
type Parser a = Parsec.Parsec String () a

-- a bag is word space word ++ " bag(s)", eg "faded tomato bags"
bagParser :: Parser Bag
bagParser = do
  part1 <- Parsec.many1 Parsec.letter
  Parsec.space
  part2 <- Parsec.many1 Parsec.letter
  Parsec.space
  Parsec.string "bag"
  Parsec.many $ Parsec.char 's' -- 0 or more 's'
  return $ part1 ++ " " ++ part2

-- number followed by a bag
countBagParser :: Parser (Count,Bag)
countBagParser = do
  count <- Parsec.many1 Parsec.digit
  Parsec.spaces
  bag <- bagParser
  return (read count, bag)

-- comma separated, spaces are allowed before and after
separatorParser :: Parser ()
separatorParser = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces

-- either "no other bags" or comma separated list of numbers and bags
contentsParser :: Parser [(Count,Bag)]
contentsParser = do
  Parsec.string "no other bags" >> return []
  <|>
  Parsec.sepBy countBagParser separatorParser

-- a rule is: bag "contain" contents "."
ruleParser :: Parser Rule
ruleParser = do
  bag <- bagParser
  Parsec.spaces
  Parsec.string "contain"
  Parsec.spaces
  contents <- contentsParser
  Parsec.char '.'
  return (bag, contents)

-- all bags and their counts that a bag can directly contain
-- concat gets rid of the 'Just', because lookup returns Maybe
bagContents :: Rules -> Bag -> [(Count,Bag)]
bagContents rules bag = concat $ lookup bag rules

-- using given rules, how many of bag can container directly contain
canContain :: Rules -> Bag -> Bag -> Count
canContain rules bag container = sum counts
  where contents  = bagContents rules container
        counts    = map fst $ filter (\cb -> snd cb == bag) contents

-- how many of bag can container eventually contain
canEventuallyContain :: Rules -> Bag -> Bag -> Count
canEventuallyContain rules bag container =
  dc + (sum $ map subcontain contents)
  where dc            = canContain rules bag container
        contents      = bagContents rules container
        subcontain cb = (fst cb) * canEventuallyContain rules bag (snd cb)

-- number of bags that eventually go into this bag
countEventualContents :: Rules -> Bag -> Count
countEventualContents rules bag = dc + cec
  where contents    = bagContents rules bag
        dc          = sum $ map fst contents
        mulcount cb = (fst cb) * countEventualContents rules (snd cb)
        cec         = sum $ map mulcount contents

-- convenience function
-- "(input)" is a string used for error reporting by Parsec
-- useful if you're parsing multiple files, to see in which file the parsing problem is
-- we, however, are only parsing one file, so we conveniently keep it constant
parse rule text = Parsec.parse rule "(input)" text

-- part 1 : how many of all bags can eventually contain at least 1 "shiny gold" bag
part1 :: Rules -> Count
part1 rules = length $ filter (>0) cec
  where bags  = map fst rules
        cec   = map (canEventuallyContain rules "shiny gold") bags

-- part 2 : how many individual bags go into a "shiny gold" bag
part2 :: Rules -> Count
part2 rules = countEventualContents rules "shiny gold"

main = do
  rulestxt <- lines <$> readFile "input7.txt"

  let rules = rights $ map (parse ruleParser) rulestxt

  print $ part1 rules
  print $ part2 rules
