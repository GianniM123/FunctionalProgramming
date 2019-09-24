-- Gianni Monteban & Martijn Vogelaar
-- s1047546 & s1047391

module Main
where
import Data.Char

-- 2.2
everyCharIsEqual :: String -> String -> Int -> Bool
everyCharIsEqual x y c
 | c == length x = True
 | toLower (x !! c) == toLower (y !! c) = everyCharIsEqual (x) (y) (c+1)
 | otherwise = False

equal      :: String -> String -> Bool
equal x y
 | length x == length y = everyCharIsEqual x y 0
 | otherwise = False

isNumeral  :: String -> Bool
isNumeral x = all isDigit x

equalSpace      :: Char -> Bool
equalSpace x = x == ' '

isBlank    :: String -> Bool
isBlank x = all equalSpace x

fromDigit  :: Char -> Int
fromDigit c 
 | ord c >= ord '0' && ord c <= ord '9' = ord c - ord '0'
 | otherwise = error("char is not a digit " ++ show c)

toDigit    :: Int -> Char
toDigit i
 | i >= 0 && i <= 9 = chr (ord '0' + i)
 | otherwise = error("int can't be converted to char " ++ show i)


shift      :: Int -> Char -> Char
shift x c 
 | not (equalSpace c) =  chr (((ord c - 65 + x) `mod` 26) + 65)
 | otherwise = ' '

msg  ::  String
msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
        \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"


stringDecoder :: String -> Int -> String
stringDecoder x y = map (shift y ) x



main = do
    print (equal "JoE" "joe")
    print (equal "hello" "hi   ")
    print (isNumeral "555555")
    print (isNumeral "52ss25")
    print (isBlank "         ")
    print (isBlank " no     ")
    print (fromDigit '5')
    print (toDigit 1)
    putStrLn (stringDecoder msg 19)

