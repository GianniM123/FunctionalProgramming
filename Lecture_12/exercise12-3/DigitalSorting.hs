{-
Gianni Monteban & Martijn Vogelaar
1047546 & 1047391
-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (lookup)
import Data.Maybe

toMaybe :: Map () a -> Maybe a
toMaybe Empty      = Nothing
toMaybe (Single v) = Just v

class Key key where
   data Map key :: * -> *

   empty  ::  Map key val
   lookup ::  key -> Map key val -> Maybe val
   insert ::  key -> (Maybe val -> val) -> Map key val -> Map key val
   
instance Key () where
   data Map () val = Empty | Single val

-- empty  ::  Map () val
   empty = Empty
   
-- lookup ::  () -> Map () val -> Maybe val
   lookup ()  = toMaybe

   insert () f = Single . f . toMaybe 


instance (Key key1, Key key2) => Key (Either key1 key2) where
   data Map (Either key1 key2) val  = M (Map key1 val, Map key2 val)

-- empty  ::  (Key key1, Key key2) => Map (Either key1 key2) val
   empty = M (empty,empty)

-- lookup ::  (Key key1, Key key2) => Either key1 key2 -> Map (Either key1 key2) val -> Maybe val
   lookup (Left k) (M (m,n)) = lookup k m
   lookup (Right k) (M (m,n)) = lookup k n
-- insert ::  key -> (Maybe val -> val) -> Map (Either key1 key2) val -> Map (Either key1 key2) val
   insert (Left k) f (M (m,n)) = M (insert k f m, n)
   insert (Right k) f (M (m,n)) = M (m,insert k f n)
   
instance (Key key1, Key key2) => Key (key1, key2) where
   data Map (key1,key2) val  = Mt (Map key1 (Map key2 val))

-- empty  ::  (Key key1, Key key2) => Map (key1, key2) val
   empty = Mt (empty)

-- lookup ::  (Key key1, Key key2) => (key1, key2) -> Map (key1, key2) val -> Maybe val
   lookup (k1, k2) (Mt m) = case (lookup k1 m) of
                            Nothing -> Nothing
                            Just x -> lookup k2 x 




-- insert ::  key -> (Maybe val -> val) -> Map (key1, key2) val -> Map (key1, key2) val
   insert (k1, k2) f (Mt m) = Mt $ insert k1 (insertHelper k2 f) m

insertHelper :: (Key key) => key -> (Maybe val -> val) -> Maybe (Map key val) -> Map key val
insertHelper k f Nothing = insert k f empty
insertHelper k f (Just x) = insert k f x

type List elem  =  Either () (elem, [elem])

toList :: [elem] -> List elem
toList []        =  Left ()
toList (a : as)  =  Right (a, as)

instance (Key key) => Key [key] where
   data Map [key] val = Ms (Map (List key) val)

   empty = Ms empty

   lookup l (Ms t) = lookup (toList l) t

   insert l f (Ms t) = Ms $ insert (toList l) f t

data Base = A | T | C | G
   deriving (Show)

type BASE = (Either () (), Either () ())

toBase A = (Left (), Left ()) 
toBase T = (Left (), Right ())
toBase C = (Right (), Left ())
toBase G = (Right (), Right ())

instance Key Base where
   data Map Base val = B (Map BASE val)

   empty            = B empty
   
   lookup k (B m)   = lookup (toBase k) m
  
   insert k f (B m) = B $ insert (toBase k) f m   