

data Expr
  = Var String
  | App Expr Expr
  | Abs String Expr
  deriving (Show)

data FExpr a
  = FVar String
  | FApp a a
  | FAbs String a
  deriving (Show)

--type Expr' = FExpr Expr'

newtype Fix f = Fix (f (Fix f))
type Expr' = Fix FExpr

instance Functor FExpr where
  fmap _ (FVar x) = FVar x
  fmap f (FApp a b) = FApp (f a) (f b)
  fmap f (FAbs x a) = FAbs x (f a)

-- f :: * -> *
ffold :: Functor f => (f a -> a) -> Fix f -> a
ffold g (Fix x) = g (fmap (ffold g) x)
  -- x :: f (Fix f)
  -- fmap _something x :: f a
  -- _something :: Fix f -> a

-- \x -> \y -> y x
example1 :: Expr
example1 = Abs "x" (Abs "y" (App (Var "x") (Var "y")))

example1' :: Expr'
example1' = Fix (FAbs "x" (Fix (FAbs "y" (Fix (FApp (Fix (FVar "x")) (Fix (FVar "y")))))))

countVars :: Expr -> Int
countVars (Var _) = 1
countVars (App a b) = countVars a + countVars b
countVars (Abs _ a) = countVars a

countVars' :: Expr' -> Int
countVars' = ffold count

count :: FExpr Int -> Int
count (FVar _) = 1
count (FApp a b) = a + b
count (FAbs _ a) = a


out :: Expr -> FExpr Expr
out (Var x)   = FVar x
out (App a b) = FApp a b
out (Abs x a) = FAbs x a

inn :: FExpr Expr -> Expr
inn (FVar x) = Var x
inn (FApp a b) = App a b
inn (FAbs x a) = Abs x a

ffold2 :: (FExpr a -> a) -> Expr -> a
ffold2 g x = g (fmap (ffold2 g) (out x))

countVars2 :: Expr -> Int
countVars2 = ffold2 count


magic :: Int -> FExpr Int
magic i
 | i < 10 = FVar (show i)
 | even i = FAbs (show i) (i `div` 2)
 | otherwise = FApp (i `div` 3) (i `div` 2 + 1)

funfold :: (a -> FExpr a) -> a -> Expr
funfold g x = inn (fmap (funfold g) (g x))


















