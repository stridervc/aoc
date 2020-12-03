import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)

type Password = T.Text

data Policy = Policy
  { val1 :: Int
  , val2 :: Int
  , chR  :: Char
  } deriving (Eq, Show)

-- helper function to convert text to int
-- assuming it can be parsed
t2i :: T.Text -> Int
t2i t = do
  case decimal t of
    Left _  -> 0
    Right n -> fst n

-- quick and dirty parse text to (Policy,Password)
parseLine :: T.Text -> (Policy, Password)
parseLine ts = (policy,password)
  where spl       = T.split (==':') ts
        left      = spl!!0
        password  = T.dropWhile (==' ') $ spl!!1
        v1        = t2i $ T.takeWhile (/='-') left
        v2        = t2i $ T.takeWhile (/=' ') $ (T.split(=='-') left)!!1
        ch        = T.last left
        policy    = Policy {val1 = v1, val2 = v2, chR = ch}

-- count occurences of Char in Text
countChar :: Char -> T.Text -> Int
countChar c ts
  | T.null ts       = 0
  | T.head ts == c  = 1 + (countChar c $ T.tail ts)
  | otherwise       = countChar c $ T.tail ts

-- check if a password meets policy requirements
valid :: (Policy, Password) -> Bool
valid (pol,pass) = (n >= val1 pol) && (n <= val2 pol)
  where n = countChar (chR pol) pass

-- avoid importing a lib for xor
xor True True   = False
xor False False = False
xor _ _         = True

-- check if password meets 2nd policy requirements
valid2 :: (Policy, Password) -> Bool
valid2 (pol,pass)
  | (val2 pol) > n  = False
  | otherwise       = (c1 == c) `xor` (c2 == c)
  where c   = chR pol
        c1  = T.index pass $ (val1 pol) - 1
        c2  = T.index pass $ (val2 pol) - 1
        n   = T.length pass

main = do
  contents <- TIO.readFile "input2.txt"
  let entries = map parseLine $ T.lines contents
  let valids  = map valid entries
  let valid2s = map valid2 entries

  putStrLn $ show $ length $ filter id valids
  putStrLn $ show $ length $ filter id valid2s
