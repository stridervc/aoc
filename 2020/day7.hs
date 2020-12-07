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

-- a bag is word space word "bag(s)", eg "faded tomato bags"
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
separator :: Parsec.Parsec String () ()
separator = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces

-- either "no other bags" or comma separated list of numbers and bags
contentsParser :: Parsec.Parsec String () [(Count,Bag)]
contentsParser = do
  Parsec.string "no other bags" >> return []
  <|>
  Parsec.sepBy countBagParser separator

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
parse rule text = Parsec.parse rule "input7.txt" text

-- all bags that a bag can directly contain
bagContents :: [Rule] -> Bag -> [Bag]
bagContents rules bag = map snd crules
  where Just crules = lookup bag rules

-- using given rules, can bag contain bag
canContain :: [Rule] -> Bag -> Bag -> Bool
canContain rules container containee = containee `elem` contents
  where contents  = bagContents rules container

canEventuallyContain :: [Rule] -> Bag -> Bag -> Bool
canEventuallyContain rules container containee
  | can       = True
  | otherwise = and $ map cec contents
  where can       = canContain rules container containee
        contents  = bagContents rules container
        cec       = (\c -> canEventuallyContain rules c containee)

main = do
  rulestxt <- lines <$> readFile "input7.txt"

  let rules = rights $ map (parse ruleParser) rulestxt
  let bags  = map fst rules

  print $ canEventuallyContain rules "striped crimson" "pale orange"
  -- TODO instead of bool can contain, return how many of that bag it can contain
