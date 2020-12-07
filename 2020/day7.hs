{-# LANGUAGE FlexibleContexts #-}

import Data.List.Split (splitOn)
import Data.Either (rights)

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))

type Bag    = String
type Count  = Int
type Rule   = (Bag, [(Count,Bag)])

{- example rules
 - mirrored violet bags contain 1 bright beige bag.
 - dim olive bags contain no other bags.
 - striped crimson bags contain 1 pale orange bag, 5 dim white bags, 3 clear fuchsia bags.
 -}

-- a bag is word space word ++ " bag(s)", eg "faded tomato bags"
bagParser :: Parsec.Parsec String () Bag
bagParser = do
  part1 <- Parsec.many1 Parsec.letter
  Parsec.space
  part2 <- Parsec.many1 Parsec.letter
  Parsec.spaces
  Parsec.string "bag"
  Parsec.many $ Parsec.char 's' -- 0 or more 's'
  return $ part1 ++ " " ++ part2

-- number followed by a bag
countBagParser :: Parsec.Parsec String () (Count,Bag)
countBagParser = do
  count <- Parsec.many1 Parsec.digit
  Parsec.spaces
  bag <- bagParser
  return (read count, bag)

-- comma separated, spaces are ok
separatorParser :: Parsec.Parsec String () ()
separatorParser = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces

-- either "no other bags" or comma separated list of numbers and bags
contentsParser :: Parsec.Parsec String () [(Count,Bag)]
contentsParser = do
  Parsec.string "no other bags" >> return []
  <|>
  Parsec.sepBy countBagParser separatorParser

-- a rule is: bag "contain" contents
ruleParser :: Parsec.Parsec String () Rule
ruleParser = do
  bag <- bagParser
  Parsec.spaces
  Parsec.string "contain"
  Parsec.spaces
  contents <- contentsParser
  return (bag, contents)

-- convenience function
parse rule text = Parsec.parse rule "(input)" text

-- all bags and their counts, that a bag can directly contain
bagContents :: [Rule] -> Bag -> [(Count,Bag)]
bagContents rules bag = concat $ lookup bag rules

-- using given rules, how many of bag can container directly contain
canContain :: [Rule] -> Bag -> Bag -> Count
canContain rules bag container = sum counts
  where contents  = bagContents rules container
        counts    = map fst $ filter (\cb -> snd cb == bag) contents

-- how many of bag can container eventually contain
canEventuallyContain :: [Rule] -> Bag -> Bag -> Count
canEventuallyContain rules bag container =
  dc + (sum $ map subcontain contents)
  where dc          = canContain rules bag container
        contents    = bagContents rules container
        subcontain  = (\cb -> (fst cb) * canEventuallyContain rules bag (snd cb))

countNotZero :: [Int] -> Int
countNotZero = length . filter (/=0)

main = do
  rulestxt <- lines <$> readFile "input7.txt"

  let rules = rights $ map (parse ruleParser) rulestxt
  let bags  = map fst rules

  -- Part One --
  print $ countNotZero $ map (canEventuallyContain rules "shiny gold") bags

  -- Part Two --

