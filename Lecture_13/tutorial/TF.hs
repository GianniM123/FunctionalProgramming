{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

data Nat = Zero | Succ Nat

data ZERO = ZERO
data SUCC n = SUCC n
type ONE = SUCC ZERO
type TWO = SUCC ONE
type THREE = SUCC TWO

one :: ONE
one = SUCC ZERO
two :: TWO
two = SUCC one


type family NArgsOf n a b
type instance NArgsOf ZERO a b = b
type instance NArgsOf (SUCC n) a b = a -> NArgsOf n a b

showNArgsOf :: Nat -> String -> String -> String
showNArgsOf Zero _ b = b
showNArgsOf (Succ n) a b = a ++ " -> " ++ showNArgsOf n a b

class Add n where
  addAcc :: n -> Int -> NArgsOf n Int Int

instance Add ZERO where
  addAcc ZERO total = total

instance Add n => Add (SUCC n) where
  addAcc (SUCC n) total x = addAcc n (total + x)

{-
-- Such an instance is not possible
instance (Add n, Add m) => Add (n,m) where
  addAcc (n,m) total = addAcc n (addAcc m total)
-}

add :: Add n => n -> NArgsOf n Int Int
add n = addAcc n 0


{-
add (SUCC (SUCC ZERO)) :: NArgsOf (SUCC (SUCC ZERO)) Int Int
   = Int -> NArgsOf (SUCC ZERO) Int Int
   = Int -> Int -> NArgsOf ZERO Int Int
   = Int -> Int -> Int
-}


--------------------

f :: stuff -> Int
f = undefined

f' :: stuff -> (Int -> res) -> res
f' stuff cont = cont (f stuff)


class Add2 n where
  addAcc2 :: n -> (Int -> res) -> Int -> NArgsOf n Int res

instance Add2 ZERO where
  addAcc2 ZERO cont total = cont total

instance Add2 n => Add2 (SUCC n) where
  addAcc2 (SUCC n) cont total x = addAcc2 n cont (total + x)

type instance NArgsOf (n,m) a b = NArgsOf n a (NArgsOf m a b)

instance (Add2 n, Add2 m) => Add2 (n,m) where
  addAcc2 (n,m) cont total = -- :: NArgsOf (n,m) Int res = NArgsOf n Int (NArgsOf m Int res)
    addAcc2 n myCont total
    where
    myCont newTotal = addAcc2 m cont newTotal
    -- cont :: Int -> res
    -- addAcc2 n myCont total :: NArgsOf n Int (NArgsOf m Int res)
    -- myCont :: Int -> (NArgsOf m Int res)
    -- 

add2 :: Add2 n => n -> NArgsOf n Int Int
add2 n = addAcc2 n id 0

--------------------------------------------------------

type family Plus a b
type instance Plus ZERO a = a
type instance Plus (SUCC a) b = SUCC (Plus a b)

data family LIST n a
data instance LIST ZERO a = Nil
data instance LIST (SUCC n) a = Cons a (LIST n a)

example :: LIST THREE Int
example = Cons 123 (Cons 456 (Cons 78 Nil))

--example2_bad :: LIST THREE Int
--example2_bad = Cons 123 (Cons 456 Nil)

class Concat m where
  concat2 :: LIST m a -> LIST n a -> LIST (Plus m n) a
instance Concat ZERO where
  concat2 Nil ys = ys
instance Concat m => Concat (SUCC m) where
  concat2 (Cons x xs) ys = Cons x (concat2 xs ys)












