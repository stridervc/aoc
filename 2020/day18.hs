import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

data Eval = Value Int | Add Eval Eval | Mul Eval Eval
  deriving (Eq, Show)

eval :: Eval -> Int
eval (Value n)  = n
eval (Add a b)  = eval a + eval b
eval (Mul a b)  = eval a * eval b

operator :: Parser Char
operator = do
  P.spaces
  op <- P.char '+' <|> P.char '*'
  P.spaces
  return op

evalParser :: Parser Eval
evalParser  =
  P.try (do
    num <- P.many1 P.digit
    op <- operator
    expr <- evalParser
    P.spaces
    case op of
      '+' -> return $ Add (Value $ read num) expr
      _   -> return $ Mul (Value $ read num) expr
    )
  <|> P.try (do
    _ <- P.char '('
    P.spaces
    expr <- evalParser
    P.spaces
    _ <- P.char ')'
    return expr
    )
  <|> (do
    num <- P.many1 P.digit
    P.spaces
    return $ Value $ read num
    )

main :: IO ()
main = do
  let example1 = "2 * 3 + (4 * 5)"

  print example1
  print $ eval $ Add (Mul (Value 2) (Value 3)) (Mul (Value 4) (Value 5))

  let e = P.parse evalParser "(example1)" example1
  case e of
    Left err    -> print err
    Right expr  -> do
      print expr
      print $ eval expr
