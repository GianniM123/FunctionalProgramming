module MapReduce
where
import Hardware
import Data.Monoid

reduce  ::  (Monoid m) => [m] -> m
reduce  =  foldr (<>) mempty 

-- instance Monoid Bool where

newtype OrdList elem = Ord [elem]

-- instance (Ord elem) => Monoid (OrdList elem) where

-- foldm :: (a -> a -> a) -> a -> ([a] -> a)

kpg :: (Bit, Bit) -> (Carry -> Carry)
kpg (O,  O  )  =  \ _c  -> O  -- kill
kpg (O,  I  )  =  \ c   -> c  -- propagate
kpg (I,  O  )  =  \ c   -> c  -- propagate
kpg (I,  I  )  =  \ _c  -> I  -- generate

data KPG  =  K | P | G
