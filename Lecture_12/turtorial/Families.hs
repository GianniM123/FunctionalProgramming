{-# LANGUAGE TypeFamilies #-}
import Prelude hiding (fst)

class First a where
  type FirstPart a
  fst :: a -> FirstPart a

instance First (a,b) where
  type FirstPart (a,b) = a
  fst (x,_) = x

instance First (a,b,c) where
  type FirstPart (a,b,c) = a
  fst (x,_,_) = x


