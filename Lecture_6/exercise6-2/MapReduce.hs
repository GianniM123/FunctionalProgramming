module MapReduce
where
import Hardware
import Data.Monoid
import Data.List

reduce  ::  (Monoid m) => [m] -> m
reduce  =  foldr (<>) mempty 

{-
x || y              OR
!x || y
x || !y
!x || !y
x && y              AND
!x && y
x && !y
!x && !y
x
y
!x
!y
True
False
x || y && not x && y
!(x || y && not x && y)
-}


newtype Bool1 = B1 {bool :: Bool }
    deriving (Show)

instance Monoid Bool1 where
    mempty = B1 False
    x `mappend` y = B1 (bool x Prelude.|| bool y)



newtype Additive = S1 { fromSum :: Int}
    deriving (Show)

instance Monoid Additive where
    mempty = S1 0
    x `mappend` y = S1 (fromSum x + fromSum y)


newtype OrdList elem = Ord {list::[elem]}
    deriving (Show)

instance (Ord elem) => Monoid (OrdList elem) where
    mempty = Ord []
    x `mappend` y = Ord ( sort (list x ++ list y))


-- instance (Ord elem) => Monoid (OrdList elem) where

-- foldm :: (a -> a -> a) -> a -> ([a] -> a)

kpg :: (Bit, Bit) -> (Carry -> Carry)
kpg (O,  O  )  =  \ _c  -> O  -- kill
kpg (O,  I  )  =  \ c   -> c  -- propagate
kpg (I,  O  )  =  \ c   -> c  -- propagate
kpg (I,  I  )  =  \ _c  -> I  -- generate

data KPG  =  K | P | G
