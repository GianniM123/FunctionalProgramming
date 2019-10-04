module Hardware
where

data Bit  =  O | I
  deriving (Eq, Ord, Show)

infixr 3 ∧∧
(∧∧) :: Bit -> Bit -> Bit
O ∧∧ _b  =  O
I ∧∧ b   =  b

infixr 2 ||
(||) :: Bit -> Bit -> Bit
O || b   =  b
I || _b  =  I

infixr 4 ><
(><) :: Bit -> Bit -> Bit
O >< O  =  O
O >< I  =  I
I >< O  =  I
I >< I  =  O


mapr :: ((a, state) -> (b, state)) -> (([a], state) -> ([b], state))
mapr f = \(as, s) -> foldr g ([],s) as
  where g a (bs, s) = let (b, sn) = f (a, s) in (b:bs, sn)

  --(map (\z -> fst (f (z,y))) (x), (snd (f (head (x), y)) ))

type Carry  =  Bit

halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (x,y) = (x >< y, x ∧∧ y)


fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((x,y),c) = (fst b , snd a Hardware.|| snd b)
  where a = halfAdder (x,y)
        b = halfAdder (fst a, c)

rippleAdder :: ([(Bit,Bit)], Carry) -> ([Bit],Carry)
rippleAdder = mapr (fullAdder)