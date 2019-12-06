import Parser
import Control.Applicative


sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
  x <- p
  (do
    _ <- sep
    xs <- p `sepBy` sep
    return (x:xs)) <|> return [x]

sepByOrEmpty :: Parser a -> Parser b -> Parser [a]
sepByOrEmpty p sep = p `sepBy` sep <|> return []

-- "[1,2,3]"
intList :: Parser [Int]
intList = do
  _ <- char '['
  xs <- int `sepByOrEmpty` (char ',')
  _ <- char ']'
  return xs

-------------------------------------------------


intList' :: Parser [Int]
intList' = combine <$> char '[' <*> int `sepByOrEmpty` (char ',') <*> char ']'
  where
  combine _ xs _ = xs

{-
combine <$> char '[' :: Parser ([Int] -> Char -> [Int])
.. <*> _ `sepBy` _   :: Parser (Char -> [Int])

-}

intList'' :: Parser [Int]
intList'' = char '[' *> int `sepByOrEmpty` (char ',') <* char ']'


-------------------------------------------------

data Expr = Lit Int | Add Expr Expr
  deriving (Show)

expr :: Parser Expr
expr = --Add <$> expr <* char '+' <*> expr
       do
          a <- exprLit
          char '+'
          b <- expr
          return (Add a b)
   <|> exprLit
  where
  exprLit = Lit <$> int

