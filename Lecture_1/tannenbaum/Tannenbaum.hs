module Main
where

row :: Int -> Int-> String
row 0 0 = ['\n']
row 0 y = ['*'] ++ row (0) (y-1)
row x y = [' '] ++ row (x-1) (y)

-- (y-1) - (x `div` 2) this calculates the number of spaces needed for the triangle
loopForRow :: Int -> Int -> Int-> String
loopForRow x y spaces
 | x <= (y * 2) = row (((y-1) - (x `div` 2)) + spaces) (x) ++ loopForRow (x + 2) (y) (spaces)
 | otherwise = ""

triangle :: Int -> String
triangle n = loopForRow 1 n 0

triangleWithSpaces :: Int -> Int -> String
triangleWithSpaces n spaces = loopForRow 1 n spaces

loopThroughTriangle :: Int -> Int -> Int -> String
loopThroughTriangle index m spacing
 | index <= m = triangleWithSpaces (index) (spacing) ++ loopThroughTriangle (index+1) (m) (spacing-1)
 | otherwise = ""

christmasTree :: Int -> String
christmasTree n = loopThroughTriangle (0) (n) (n)

main = do
    putStrLn (triangle 5)
    putStrLn (christmasTree 4)

