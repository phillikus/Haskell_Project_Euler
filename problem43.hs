import Data.List

panDigitals = permutations "0123456789"

isDivisibleBy n t = mod n t == 0

toInt :: String -> Integer
toInt = read

toIntList :: [String] -> [Integer]
toIntList = map read

hasProperty str = (isDivisibleBy (toInt ([str !! 1] ++ [str !! 2] ++ [str !! 3])) 2) &&
                  (isDivisibleBy (toInt ([str !! 2] ++ [str !! 3] ++ [str !! 4])) 3) &&
                  (isDivisibleBy (toInt ([str !! 3] ++ [str !! 4] ++ [str !! 5])) 5) &&
				  (isDivisibleBy (toInt ([str !! 4] ++ [str !! 5] ++ [str !! 6])) 7) &&
				  (isDivisibleBy (toInt ([str !! 5] ++ [str !! 6] ++ [str !! 7])) 11) &&
				  (isDivisibleBy (toInt ([str !! 6] ++ [str !! 7] ++ [str !! 8])) 13) &&
				  (isDivisibleBy (toInt ([str !! 7] ++ [str !! 8] ++ [str !! 9])) 17)
				  
main = print $ sum $ toIntList $ filter (hasProperty) panDigitals