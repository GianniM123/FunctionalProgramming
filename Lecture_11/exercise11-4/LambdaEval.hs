{-
Gianni Monteban & Martijn Vogelaar
1047546 & 1047391
-}

import Data.Maybe
import Prelude hiding (Either, Right, Left)
infixl 9 :@:

data Expr
  =  Lit Integer      -- a literal
  |  Var String       -- a variable
  |  Bin Expr Op Expr -- binary operator
  |  Abs String Expr  -- a lambda expression
  |  Expr :@: Expr    -- an application
  deriving Show

data Op = Plus | Mult
  deriving Show

data Value = IntVal Integer | FunVal Env String Expr
  deriving Show
  
type Env = [(String,Value)]

data Either e a = Left e | Right a deriving (Show)

applyIntOp :: Op -> Value -> Value -> Value
applyIntOp op (IntVal v1) (IntVal v2) = 
   case op of 
      Plus -> IntVal (v1 + v2)
      Mult -> IntVal (v1 * v2)

eval0 :: Expr -> Env -> Value
eval0 (Lit i)        env = IntVal i
eval0 (Var v)        env = fromJust (lookup v env)
eval0 (Bin e1 op e2) env = applyIntOp op (eval0 e1 env) (eval0 e2 env)
eval0 (Abs v b)      env = FunVal env v b
eval0 (ef :@: ea)    env = let FunVal env var body = eval0 ef env
                               arg                 = eval0 ea env
                           in eval0 body ((var,arg):env) 

myExpr = Bin (Lit 12) Plus ((Abs "x" (Bin (Var "x") Mult (Lit 2))) :@: Bin (Lit 4) Plus (Lit 2))
myExprF = Bin (Lit 12) Plus ( Bin (Lit 4) Plus (Lit 2) :@:(Abs "x" (Bin (Var "x") Mult (Lit 2))) )

newtype Environ a = EN { fromEN :: Env ->  a }

instance Functor Environ where
-- fmap :: (a -> b) -> Environ a -> Environ b
   fmap f (EN x) = EN $ \env -> f (x env) 

instance Applicative Environ where
-- pure a -> Environ a
   pure x = EN $ \env -> x 
    
-- (<*>) :: Environ (a -> b) -> Environ a -> Environ b 
   EN f <*> EN x  = EN $ \ev -> func ev
    where func = \env -> f env (x env)

instance Monad Environ where
  -- return a -> Environ a
  return     = pure
  -- (>>=) :: Environ a -> (a -> Environ b) -> Environ b =
  EN m >>= f = EN $ \env -> (fromEN (f (m env)) env)

eval1 ::Expr -> Environ Value
eval1 (Lit i)        = return (IntVal i)
eval1 (Var v)        = EN $ \env -> fromJust (lookup v env)
eval1 (Bin e1 op e2) = eval1 e1 >>= \n1 ->
                       eval1 e2 >>= \n2 ->
                        return(applyIntOp op n1 n2)
eval1 (Abs v b)      = EN $ \env -> FunVal env v b
eval1 (ef :@: ea)    = eval1 ef >>= \n1 ->
                       eval1 ea >>= \n2 ->
                       let FunVal env var body = n1
                        in return(fromEN (eval1 body) [(var, n2)])
    
eval2 :: Expr -> Environ (Either String Value)
eval2 (Lit i)        = return $ Right(IntVal i)
eval2 (Var v)        = EN $ \env -> case lookup v env of
                                    Nothing -> Left "Variable does not exsists"
                                    Just x -> Right (x)
eval2 (Bin e1 op e2) = eval2 e1 >>= \n1 ->
                       eval2 e2 >>= \n2 ->
                        case n1 of
                        Left s ->return(Left s)
                        Right xf -> case n2 of
                          Left z -> return(Left z)
                          Right yf -> case xf of
                            FunVal _ _ _ -> return (Left("Why give me a fuction here"))
                            x -> case yf of
                              FunVal _ _ _ -> return (Left("WHy give me a fucntion here"))
                              y -> return(Right(applyIntOp op x y))
eval2 (Abs v b)      = EN $ \env -> Right(FunVal env v b)
eval2 (ef :@: ea)    = eval2 ef >>= \n1 ->
                       eval2 ea >>= \n2 ->
                       case n1 of
                          Left s ->return(Left s)
                          Right x -> case n2 of
                            Left z -> return(Left z)
                            Right y ->  case x of
                                          IntVal x -> return (Left("Why do you give a integer here?"))
                                          FunVal env var body -> return(fromEN (eval2 body) [(var, y)])
