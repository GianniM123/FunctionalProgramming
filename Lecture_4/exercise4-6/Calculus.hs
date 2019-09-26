module Calculus
where

data Primitive
  =  Sin  -- trigonometric: sine
  |  Cos                          -- cosine
  |  Exp  -- exponential
  deriving (Show,Eq)

infixl 6 :+:
infixl 7 :*:
infixr 9 :.:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Prim Primitive         -- primitive function
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :.: Function  -- composition of functions
  deriving (Show, Eq)


apply    :: Function -> (Double -> Double)
apply (f :+: g) v = apply f v + apply g v
apply (f :*: g) v = apply f v * apply g v
apply (f :.: g) v = apply f (apply g v)
apply (Prim p) v
 | p == Sin = sin(v)
 | p == Cos = cos(v)
 | p == Exp = exp(v)
 |otherwise = 0
apply (Const c) v = fromRational (c)
apply i v = v


derive   :: Function -> Function
derive (f :+: g) = derive f :+: derive g 
derive (f :*: g) = derive f :*: g :+: derive g :*: f
derive (Prim p)
 | p == Sin = Prim Cos
 | p == Cos = Const (-1) :*: Prim Sin
 | p == Exp = Prim Exp
derive (Const c) = Const (0)
derive i = Const (1)


simplify :: Function -> Function
simplify (Const 0 :*: _) = Const 0
simplify (_ :*: Const 0) = Const 0
simplify (Const x :*: Const y) = Const (x*y)
simplify (x :*: y) = simplify (simplify x :*: simplify y)
simplify (Const x :+: Const y) =  Const (x+y)
simplify (Const (0) :+: f) = f
simplify (f :+: Const (0)) = f
simplify ((f) :+: (g)) = if (f==g) then (Const (2) :*: (g))  else ((f) :+: (g)) 
simplify (f) = f