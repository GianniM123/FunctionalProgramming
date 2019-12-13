{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- printf D 51
-- printf ("I am " & D & " years old.") 51
-- printf ("I am " & D & " " & S & " old.") 1 "year"
-- fmt = "Color " & S & ", Number " & D & ", Float " & F
-- printf fmt "purple" 4711 3.1415
