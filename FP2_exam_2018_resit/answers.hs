{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Examination1
where
import Prelude hiding (and, or)
--import Unicode
import Control.Monad (ap)
import Data.List
import Data.String
--import Squiggol

main :: IO ()
main = putStr "Hello World!"

---------------------------------------------------------------------
--  Formula's and (e)valuation
---------------------------------------------------------------------
data Formula var
  =  Var  var
  |  Not  (Formula var)
  |  And  (Formula var) (Formula var)
  |  Or   (Formula var) (Formula var)
type Environment var val = [(var,val)]

lookup' :: (Eq var) => var -> Environment var val -> Maybe val
lookup' var [] = Nothing
lookup' var ((var',val) : env)
  | var == var'  = Just val
  | otherwise    = lookup' var env

eval :: (Eq var) => Environment var Bool -> Formula var -> Maybe Bool
eval env (Var var) = lookup' var env
eval env (Not f1)  = fmap not (eval env f1)
eval env (And f1 f2) = eval env f1 >>= \v1 ->
                       eval env f2 >>= \v2 -> 
                       return (v1 && v2)
eval env (Or  f1 f2) = eval env f1 >>= \v1 ->
                       eval env f2 >>= \v2 -> 
                       return (v1 || v2)

vars :: (Eq var) => Formula var -> [var]
vars (Var var) = [var]
vars (Not f1) = vars f1
vars (And f1 f2) = nub (vars f1 ++ vars f2)
vars (Or f1 f2)  = nub (vars f1 ++ vars f2)

all_valuations :: [var] -> [val] -> [Environment var val]
all_valuations [] _ = [[]]
all_valuations (var : vars) vals = concat [ concat [ [((var,val):valuation)] | val <- vals] | valuation <- all_valuations vars vals]

is_true (Just True) = True
is_true _ = False

truths :: (Eq var) => Formula var -> [Environment var Bool]
truths f = [env | env <- all_valuations (vars f) [True,False], is_true (eval env f)]

---------------------------------------------------------------------
--  Evaluation strategies
---------------------------------------------------------------------
my_fun = double (length' [length' [0]])
double x = x + x

length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

{- applicative order:
  my_fun
= double (length' [length' [0]])
= double (length' [1 + length' []])
= double (length' [1 + 0])
= double (length' [1])
= double (1 + length' [])
= double (1 + 0)
= double 1
= 1 + 1
= 2


normal order:
  my_fun
= double (length' [length' [0]])
= length' [length' [0]] + length' [length' [0]]
= (1 + length' []) + length' [length' [0]]
= (1 + 0) + length' [length' [0]]
= 1 + length' [length' [0]]
= 1 + (1 + length' [])
= 1 + (1 + 0)
= 1 + 1
= 2

lazy:
  my_fun
= double (length' [length' [0]])
= let x = length' [length' [0]] in x + x
= let x = 1 + length' [] in x + x
= let x = 1 + 0 in x + x
= let x = 1 in x + x
= 1 + 1
= 2
-}

---------------------------------------------------------------------
--  Imperative programming
---------------------------------------------------------------------
type Commands = [(Command, Action)]
type Command  = String
type Argument = String
type Action   = [Argument] -> IO ()

doCommands :: Commands -> IO ()
doCommands commands
  = do 
      putStr "> "
      line <- getLine
      let ws = words line
      if null ws
      then putStrLn "goodbye"
      else let cmd  = head ws
               args = tail ws
            in case lookup' cmd commands of
                  Just io -> do io args
                                doCommands commands
                  nothing -> do putStrLn (cmd ++ " is not a known command")
                                doCommands commands

theCommands = [("echo", \args -> putStrLn (unwords args))
              ,("fib",  \args -> putStrLn (show (fib (read (head args)))))
              ]

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


------------------------------------------------------------------------
--  Functor and applicative
------------------------------------------------------------------------
data M a = M (IO a)

instance Functor M where
    fmap f (M io) = M (io >>= \x -> return (f x))

instance Applicative M where
    pure x = M (return x)
    (<*>) (M io_f) (M io_a) = M (io_f >>= \f -> io_a >>= \a -> return (f a))


------------------------------------------------------------------------
--  Folding and unfolding
------------------------------------------------------------------------
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

size :: Tree elem -> Integer
size = fold (\x -> case x of
                     EMPTY        -> 0
                     NODE sl a sr -> sl + 1 + sr)

print' :: (Show elem) => Tree elem -> String
print' = fold (\x -> case x of
                       EMPTY        -> "."
                       NODE sl a sr -> "(" ++ sl ++ ") " ++ show a ++ " (" ++ sr ++ ")")

distribute :: [elem] -> Tree elem
distribute = unfold (\values -> case values of
                                  []    -> EMPTY
                                  [x]   -> NODE [] x []
                                  more  -> NODE as (head bs) (tail bs)
                                    where n = length more
                                          (as,bs) = splitAt (div n 2) more
           )


-------------------------------------------------------------------------
--  The compact numbers problem
-------------------------------------------------------------------------
numbers :: [Int]
numbers = [11,10,5,10,6,5,1,3,8,2,2]

data Item a = Zero | One a | Two a a

instance (Show a) => Show (Item a)
  where show Zero      = ""
        show (One a)   = show a
        show (Two a b) = show a ++ "-" ++ show b

compact :: IO ()
compact = sequence_ (map (putStrLn . show . toItem) (collect (nub (sort numbers))))

toItem :: [Int] -> Item Int
toItem []   = Zero
toItem [a]  = One a
toItem more = Two (head more) (last more)

collect :: [Int] -> [[Int]]
collect []     = []
collect values = foldr add [[last values]] (init values)
  where
    add :: Int -> [[Int]] -> [[Int]]
    add new ((x:xs):done)
      | new + 1 == x = (new:x:xs):done
      | otherwise    = [new]:(x:xs):done
