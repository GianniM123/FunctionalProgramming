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

simpleInsert :: Key key => key -> val -> Map key val -> Map key val
simpleInsert k v m = insert k (\_ -> v) m

instance Key () where
   data Map () val = Empty | Single val
   empty = Empty
   lookup ()  = toMaybe
   insert () f = Single . f . toMaybe 

instance Key Bool where
   data Map Bool val = BoolMap {
      falseVal :: Maybe val,
      trueVal  :: Maybe val }
   empty = BoolMap Nothing Nothing
   lookup False (BoolMap f _) = f
   lookup True  (BoolMap _ t) = t
   insert False val (BoolMap f t) = BoolMap (Just (val f)) t
   insert True  val (BoolMap f t) = BoolMap f (Just (val t))

{-
Bool ≈ Either () ()
False = Left ()
True = Right ()
-}

instance (Key k) => Key (Maybe k) where
   data Map (Maybe k) val = MaybeMap
     { nothingVal :: Maybe val
     , justVal :: Map k val
     }
   empty = MaybeMap { nothingVal = Nothing, justVal = empty }
   lookup Nothing m = nothingVal m
   lookup (Just x) m = lookup x (justVal m)
   insert Nothing val (MaybeMap n j) = MaybeMap (Just (val n)) j
   insert (Just x) val (MaybeMap n j) = MaybeMap n (insert x val j)

instance (Key key1, Key key2) => Key (Either key1 key2) where
   data Map (Either key1 key2) val  = NotYetImplementedE

-- empty  ::  (Key key1, Key key2) => Map (Either key1 key2) val
   empty = error "empty: not yet implemented"

-- lookup ::  (Key key1, Key key2) => Either key1 key2 -> Map (Either key1 key2) val -> Maybe val
   lookup = error "lookup: not yet implemented"
   
-- insert ::  key -> (Maybe val -> val) -> Map (Either key1 key2) val -> Map (Either key1 key2) val
   insert = error "insert: not yet implemented"
   



   
instance (Key key1, Key key2) => Key (key1, key2) where
   data Map (key1,key2) val  = NotYetImplementedP

-- empty  ::  (Key key1, Key key2) => Map (key1, key2) val
   empty = error "empty: not yet implemented"

-- lookup ::  (Key key1, Key key2) => (key1, key2) -> Map (key1, key2) val -> Maybe val
   lookup = error "lookup: not yet implemented"

-- insert ::  key -> (Maybe val -> val) -> Map (key1, key2) val -> Map (key1, key2) val
   insert = error "insert: not yet implemented"

type List elem  =  Either () (elem, [elem])

toList :: [elem] -> List elem
toList []        =  Left ()
toList (a : as)  =  Right (a, as)

instance (Key key) => Key [key] where
   data Map [key] val  = NotYetImplementedL

   empty = error "empty: not yet implemented"

   lookup = error "lookup: not yet implemented"

   insert = error "insert: not yet implemented"

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
