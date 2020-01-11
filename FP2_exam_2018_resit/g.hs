{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.List

data Formula var
  =  Var  var
  |  Not  (Formula var)
  |  And  (Formula var) (Formula var)
  |  Or   (Formula var) (Formula var)
type Environment var val = [(var,val)]

eval :: (Eq var) => Environment var Bool -> Formula var -> Maybe Bool
eval env (Var v) = lookup v env
eval env (Not v) = case (eval env v) of
                   Nothing -> Nothing
                   Just x -> Just (not x)
eval env (And v1 v2) = case (eval env v1) of
                       Nothing -> Nothing
                       Just x -> case (eval env v2) of
                                 Nothing -> Nothing
                                 Just y -> Just (x && y)
eval env (Or v1 v2) = case (eval env v1) of
                       Nothing -> Nothing
                       Just x -> case (eval env v2) of
                                 Nothing -> Nothing
                                 Just y -> Just (x || y)

all_valuations :: [var] -> [val] -> [Environment var val]
all_valuations [] [] = [[]]
all_valuations _ [] = [[]]
all_valuations [] _ = [[]]
all_valuations xs ys = [[(x,y)] | x <- xs, y <-ys]
-- all_valuations [] _ = [[]]
-- all_valuations (x:[]) ys = [[(x,y)] | y <- ys]
-- all_valuations (x:xs) ys = [[(x,y)] | y <- ys] ++ all_valuations xs ys

vars :: (Eq var) => Formula var -> [var]
vars (Var var) = [var]
vars (Not f1) = vars f1
vars (And f1 f2) = nub (vars f1 ++ vars f2)
vars (Or f1 f2)  = nub (vars f1 ++ vars f2)

truths :: (Eq var) => Formula var -> [Environment var Bool]
truths = undefined


{-
applicative order
double (length [length [0]])
double (length [1 + length []])
double (length [1 + 0])
double (length [1])
double (1 + length [])
double (1 + 0)
double (1)
1 + 1
2

normal order
double (length [length[0]])
(length [length[0]]) + (length [length[0]]) 
(length [1 + length []]) + (length [length[0]]) 
(length [1 + 0]) + (length [length[0]]) 
(length [1]) + (length [length[0]]) 
(1 + length []) + (length [length[0]]) 
(1 + 0) + (length [length[0]]) 
1 + (length [1 + length []]) 
1 + (length [1 + 0]) 
1 + (length [1])
1 + (1 + length []) 
1 + (1 + 0)
1 + 1
2

lazy
double (length [length [0]])
let x = (length [length [0]]) in x + x
let x = (length [1 + length []]) in x + x
let x = (length [1 + 0]) in x + x
let x = (length [1]) in x + x
let x = (1 + length []) in x + x
let x = (1 + 0) in x + x
let x = 1 in x + x
1 + 1
2
-}


type Commands = [(Command, Action)]
type Command  = String
type Argument = String
type Action   = [Argument] -> IO ()

theCommands = [("echo2", \args -> putStrLn (unwords args))
              ,("fib",  \args -> putStrLn (show (fib (read (head args)))))
              ]

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


doCommands :: Commands -> IO ()
doCommands commands = do 
  putStr "> "
  str <- getLine
  let x = words str
  if (length x) == 0 
  then putStrLn "GoodBye" 
  else let cmd = head x
           arg = tail x
           f = lookup cmd commands
           in
        case f of
          Nothing -> do putStrLn (cmd ++ " does not exsists")
                        doCommands theCommands
          Just x -> do x arg
                       doCommands theCommands


g1 :: (Functor f) => f (a, b) -> f (b, a)
g1 = fmap (\(a,b) -> (b,a))

g2 :: (Functor f) => f (f (a, b)) -> f (f (a, b, b, a))
g2 = fmap (\x -> fmap (\(a,b) -> (a,b,b,a)) x)

g3 :: (Functor f, Functor g) => f a -> g b -> f (g (a, b))
g3 af bg = fmap (\a -> fmap (\b -> (a,b)) bg) af 

g4 :: (Applicative f, Applicative g) => a -> f (g a)
g4 a = pure (pure a)

g5 :: (Monad m) => a -> m (m a)
g5 a = return (return a)

g6 :: (Monad m) => m (m a) -> m a
g6 a = a >>= (\x -> x) 


cmdFunc :: IO()
cmdFunc = do
  doCommands theCommands


data M a = M (IO a)
instance Functor M where
  --fmap (a -> b) -> IO a -> IO b
  fmap f (M io) = M $ (io >>= \a -> return (f a))

instance Applicative M where
  -- a -> M a
  pure a = M $ return (a)

  -- M (a -> b) -> M a -> M b
  (M f) <*> (M io) = M $ (io >>= \a ->
                 f >>= \func ->
                 pure (func a))

data Tree elem      = Empty | Node (Tree elem) elem (Tree elem)
data TREE elem tree = EMPTY | NODE tree elem tree
  
class (Functor f) => Base f where
    type Rec f :: *
    inn :: f (Rec f) -> Rec f   -- tying the recursive knot
    out :: Rec f -> f (Rec f)   -- untying the recursive knot
  
instance Functor (TREE elem) where
    fmap f EMPTY        = EMPTY
    fmap f (NODE l a r) = NODE (f l) a (f r)

instance Base (TREE elem) where
  type Rec (TREE elem) = Tree elem
  inn EMPTY            = Empty
  inn (NODE l a r)     = Node l a r
  out Empty            = EMPTY
  out (Node l a r)     = NODE l a r

fold :: (Base f) => (f a -> a) -> (Rec f -> a)
fold alg = consume
  where consume = alg . fmap consume . out
  
unfold :: (Base f) => (a -> f a) -> (a -> Rec f)
unfold coalg = produce
  where produce = inn . fmap produce . coalg

print' :: (Show elem) => Tree elem -> String
print' = fold (\s -> case s of
                    EMPTY -> "."
                    NODE t1 e t2 -> "(" ++ t1 ++ ")" ++ show e ++ "(" ++ t2 ++ ")"
             )
distribute :: [elem] -> Tree elem
distribute = unfold (\l -> case l of
                           [] -> EMPTY
                           [x] -> NODE [] x []
                           more -> NODE a b bs where (a,b:bs) = splitAt ((length more) -1) more )



numbers :: [Int]
numbers = [11,10,5,10,6,5,1,3,8,2,2]

data Number a = ZERO |ONE a | TWO a a

instance (Show a) => Show (Number a) where
  show (ZERO) = ""
  show (ONE a) = show a
  show (TWO a b) = show a ++ "-" ++ show b

numbersMain :: IO()
numbersMain = sequence_ $ map (putStrLn . show) $ map (\x -> arrayToNumber x) $ ownGroupBy $ sort numbers

arrayToNumber :: [Int] -> Number Int
arrayToNumber [] = ZERO
arrayToNumber [x] = ONE x
arrayToNumber (x:xs) =  TWO x (last xs)

readUntilNotMatch :: [Int] -> [Int]
readUntilNotMatch [] = []
readUntilNotMatch (x:y:[])
 | x+1 >= y = [x] ++ [y] 
 | otherwise = [x]
readUntilNotMatch (x:y:xs)
 | x+1 >= y = [x] ++ readUntilNotMatch(y:xs)
 | otherwise = [x]

ownGroupBy :: [Int] -> [[Int]]
ownGroupBy [] = [[]]
ownGroupBy xs = let (a,b) = splitAt (length (readUntilNotMatch xs)) xs in a : ownGroupBy b
