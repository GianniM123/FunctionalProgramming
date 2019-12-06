import Parser
import Control.Applicative

infixl 6 :+:
infixl 7 :*:

data Expr
  =  Lit Int        -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  deriving (Show)

-- An expression parser using >>=.

expr, term, factor :: Parser Expr
expr    =   term
        <|>  (term >>= \ i -> char '+' >> expr >>= \ j -> return (i :+: j))
term    =   factor
        <|>  (factor >>= \ i -> char '*' >> term >>= \ j -> return (i :*: j))
factor  =   (nat >>= return . Lit)
        <|>  (char '(' >> expr >>= \ i -> char ')' >> return i)

-- Using do-notation.

expr', term', factor' :: Parser Expr
expr'    =   do term'
         <|>  do i <- term' ; char '+' ; j <- expr' ; return (i :+: j)
term'    =   do factor'
         <|>  do i <- factor' ; char '*' ; j <- term' ; return (i :*: j)
factor'  =   do n <- nat; return (Lit n)
         <|>  do char '(' ; i <- expr' ; char ')' ; return i

-- An applicative verion

exprA, termA, factorA :: Parser Expr
exprA    =   termA
        <|>  ((:+:) <$> termA <*> (char '+' *> exprA))
termA    =   factorA
        <|>  ((:*:) <$> factorA <*> (char '*' *> termA))
factorA  =   Lit <$> nat
        <|>  (char '(' *> exprA <* char ')')

-- parse expr "4*71+1"
-- parse expr "4 * 71 + 1"
