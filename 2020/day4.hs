import Data.List.Split (splitOn)
import Data.Char (isHexDigit, isNumber)

-- a passport is a list of strings, each string is "key:value"
type Passport = [String]
type Key      = String
type Value    = String

hasKey :: Passport -> Key -> Bool
hasKey p k  = k `elem` keys
  where keys  = map (head . splitOn ":") p

isValid :: Passport -> Bool
isValid p = and $ map (hasKey p) required
  where required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

isValidPair :: (Key,Value) -> Bool
isValidPair (k,v)
  | k == "byr"  = year >= 1920 && year <= 2002
  | k == "iyr"  = year >= 2010 && year <= 2020
  | k == "eyr"  = year >= 2020 && year <= 2030
  | k == "hgt"  = case unit of
    "cm"      -> h >= 150 && h <= 193
    "in"      -> h >= 59  && h <= 76
    otherwise -> False
  | k == "hcl"  = isrgb
  | k == "ecl"  = v `elem` ecls
  | k == "pid"  = len == 9 && (and $ map isNumber v)
  | k == "cid"  = True
  | otherwise   = False

  where len   = length v
        year  = read v
        h     = read $ take (len - 2) v
        unit  = drop (len - 2) v
        ecls  = ["amb","blu","brn","gry","grn","hzl","oth"]
        isrgb = head v == '#' && (len == 7) && (and $ map isHexDigit $ tail v)

toPair :: String -> (Key,Value)
toPair s  = (key, value)
  where s'    = splitOn ":" s
        key   = s'!!0
        value = s'!!1

isValid2 :: Passport -> Bool
isValid2 p = isValid p && (and $ map isValidPair pairs)
  where pairs = map toPair p

-- for Roland
(|>>) = flip (<$>)
(|>)  = flip ($)

main = do
  -- these two are equivalent
  -- passports <- map words <$> splitOn "\n\n" <$> readFile "input4.txt"
  passports <- readFile "input4.txt" |>> splitOn "\n\n" |>> map words

  -- Part One --
  -- print $ length $ filter id $ map isValid passports
  map isValid passports |> filter id |> length |> print

  -- Part Two --
  -- print $ length $ filter id $ map isValid2 passports
  map isValid2 passports |> filter id |> length |> print
