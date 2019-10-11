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
x == y
x != y
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


newtype OrdList elem = Ord [elem]
    deriving (Show)

instance (Ord elem) => Monoid (OrdList elem) where
    mempty = Ord []
    (Ord x) `mappend` (Ord y) =  Ord (foldr insert y x)


f (Ord a) = a
newSort :: (Ord a) => [a] -> [a]
newSort xs = f . reduce $ map (\x -> Ord[x]) xs


foldm :: (a -> a -> a) -> a -> ([a] -> a)
foldm f e = \x -> 

kpg :: (Bit, Bit) -> (Carry -> Carry)
kpg (O,  O  )  =  \ _c  -> O  -- kill
kpg (O,  I  )  =  \ c   -> c  -- propagate
kpg (I,  O  )  =  \ c   -> c  -- propagate
kpg (I,  I  )  =  \ _c  -> I  -- generate

data KPG  =  K | P | G
