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
derive (l :.: r) = (derive l :.: r) :*: derive r
derive (Prim p)
 | p == Sin = Prim Cos
 | p == Cos = Const (-1) :*: Prim Sin
 | p == Exp = Prim Exp
derive (Const c) = Const (0)
derive i = Const (1)


-- simplify (Id :*: Id :*: Id :*: Id :*: Const(2) :*: Const(54) :*: Const(4)  :+: Id :*: Id :*: Id :*: Id :*:  Const(53) :+: Id :*: Const(3) :*: Const(0))
simplify :: Function -> Function
simplify (Const 0 :*: _) = Const 0
simplify (_ :*: Const 0) = Const 0
simplify (Const 1 :*: f) = simplify(f)
simplify (f :*: Const 1) = simplify(f)
simplify (Const x :*: Id :+: Const y :*: Id ) = Const(x + y) :*: Id
simplify (Const x :*: Id :*: f :+: Const y :*: Id :*: g) = if(f==g) then (Const(x + y) :*: Id :*: f) else (Const x :*: Id :*: f :+: Const y :*: Id :*: g)
simplify (f :*: Const x :*: Const y) = simplify(f :*: Const (x*y))
simplify (f :*: Const x :*: Const y :+: g) = simplify(f :*: Const (x*y) :+: g)
simplify (f :*: Const x :+: g :*: Const y) = if(f==g) then (f :*: Const(x+y)) else (f :*: Const x :+: g :*: Const y)
simplify (Const x :*: Const y) = Const (x*y)
simplify (x :*: y) =  (simplify x :*: simplify y)
simplify (Const x :+: Const y) =  Const (x+y)
simplify (Const (0) :+: f) = simplify f
simplify (f :+: Const (0)) = simplify f
simplify (Const x :+: f) = Const(x) :+: simplify f
simplify (f :+: Const x) = simplify f :+: Const(x)
simplify ((f) :+: (g)) = if (f==g) then (Const (2) :*: (g))  else ((simplify(f) :+: simplify(g)))
simplify (f) = f