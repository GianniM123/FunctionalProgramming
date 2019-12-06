{-# LANGUAGE DeriveFunctor #-}
import Data.Maybe
import Control.Monad

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

type Error = String
newtype EvalM a = EvalM { unEvalM :: Env -> Either Error a }
  deriving (Functor)

instance Applicative EvalM where
  pure = return
  (<*>) = ap

instance Monad EvalM where
  return x = EvalM $ \_env -> Right x
  ex >>= ey = EvalM $ \env -> do
    x <- unEvalM ex env  -- :: Either Error a
    unEvalM (ey x) env   -- :: Either Error b

getEnv :: EvalM Env
getEnv = EvalM $ \env -> Right env

withEnv :: Env -> EvalM a -> EvalM a
withEnv newEnv ea = EvalM $ \_oldEnv -> unEvalM ea newEnv

errorMsg :: String -> EvalM a
errorMsg err = EvalM $ \_ -> Left err

applyIntOp :: Op -> Value -> Value -> EvalM Value
applyIntOp op (IntVal v1) (IntVal v2) = 
   case op of 
      Plus -> return $ IntVal (v1 + v2)
      Mult -> return $ IntVal (v1 * v2)
applyIntOp op x y = errorMsg ("Expected integers instead of " ++ show x ++ ", " ++ show y)

eval :: Expr -> EvalM Value
eval (Lit i)        =
  return $ IntVal i
eval (Var v)        = do
  env <- getEnv
  case lookup v env of
    Nothing -> errorMsg $ "Variable not found: " ++ v
    Just x  -> return x
eval (Bin e1 op e2) = do
  x1 <- eval e1
  x2 <- eval e2
  applyIntOp op x1 x2
eval (Abs v b)      = do
  env <- getEnv
  return $ FunVal env v b
eval (ef :@: ea)    = do
  f <- eval ef
  case f of
    FunVal env var body -> do
      arg <- eval ea
      withEnv ((var,arg):env) $ do
        eval body
    _ -> errorMsg $ "Expected a function instead of " ++ show f

myExpr = Bin (Lit 12) Plus ((Abs "x" (Bin (Var "x") Mult (Lit 2))) :@: Bin (Lit 4) Plus (Lit 2))


