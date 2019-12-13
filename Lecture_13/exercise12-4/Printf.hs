{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- 
Gianni Monteban & Martijn Vogelaar
1047546 & 104391
-}

module Printf
where

data D  =  D  deriving (Show)
data F  =  F  deriving (Show)
data S  =  S  deriving (Show)

infixr 4 &
(&) :: a -> b -> (a, b)
a & b  =  (a, b)

type family Arg dir res :: *

printf :: (Format dir) => dir -> Arg dir String
printf dir = format dir id ""

class Format dir where
  format :: dir -> (String -> a) -> String -> Arg dir a

type instance Arg D res  =  Int -> res

instance Format D where
  format D cont out = \ i -> cont (out ++ show i)

type instance Arg F res = Float -> res

instance Format F where
  format F cont out = \ i -> cont (out ++ show i)

type instance Arg S res = String -> res

instance Format S where
  format S cont out = \ i -> cont (out ++ i)
 
type instance Arg String res = res

instance Format String where
  format s cont out = cont (out ++ s)


type instance Arg (dir1,dir2) res = Arg dir1 (Arg dir2 res)

instance (Format dir1, Format dir2) => Format (dir1,dir2) where
  format (d1,d2) = format d1 . format d2




-- printf D 51
-- printf ("I am " & D & " years old.") 51
-- printf ("I am " & D & " " & S & " old.") 1 "year"
-- fmt = "Color " & S & ", Number " & D & ", Float " & F
-- printf fmt "purple" 4711 3.1415
