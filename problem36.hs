import Data.Char
import Numeric

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

toBin :: (Integral a, Show a) => a -> String
toBin x = showIntAtBase 2 intToDigit x ""

doublePalindromes :: [Integer]
doublePalindromes = let f x = if ((isPalindrome $ show x) && isPalindrome (toBin x)) 
                              then x : (f (x+1))
                              else f (x+1)
                    in f 0
                    
main = print $ sum $ takeWhile (<1000000) doublePalindromes