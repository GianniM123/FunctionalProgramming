module Shapes
where

data Shape
  =  Circle Double            -- radius
  |  Square Double            -- length
  |  Rectangle Double Double  -- length and width
  deriving (Show)

smallCircle, bigCircle, square, rectangle :: Shape
smallCircle = Circle (1/3)
bigCircle = Circle 2.1
square = Square pi
rectangle = Rectangle 2.0 4.0

showShape :: Shape -> String
showShape (Circle r)       =  "circle of radius " ++ show r
showShape (Square l)       =  "square of length " ++ show l
showShape (Rectangle l w)  =  "rectangle of length " ++ show l
                                ++ " and width " ++ show w

area :: Shape -> Double
area (Circle c) = pi * c^2
area (Square s) = s^2
area (Rectangle l w) = l * w

perimeter :: Shape -> Double
perimeter (Circle c) = 2 * pi * c
perimeter (Square s) = 4 * s
perimeter (Rectangle l w) = 2 * l + 2 * w

center :: Shape -> (Double, Double)  -- x- and y-coordinates
center (Circle c) = (c,c)
center (Square s) = (s/2,s/2)
center (Rectangle l w) = (l/2,w/2)

boundingBox  :: Shape -> (Double, Double)  -- width and height
boundingBox (Circle c) = (2*c, 2*c)
boundingBox (Square s) = (s,s)
boundingBox (Rectangle l w) 
 | l < w = (w,w)
 | otherwise = (l,l)