import Data.Maybe

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