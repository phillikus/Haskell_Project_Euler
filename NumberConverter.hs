module NumberConverter
(
   numToRoman,
   romanToNum
) where

import Data.Map

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

romans :: Map Int [Char]
romans = fromList [(1,"I"), (4, "IV"), (5,"V"), (9, "IX"), (10,"X"), (40, "XL"), (50, "L"),
                  (90, "XC"), (100,"C"), (400, "CD"), (500, "D"), (900, "CM"), (1000, "M")]
		
numToRoman :: Int -> [Char]
numToRoman x = resolveRoman digs (len-1)
               where
                   digs = digits x
                   len = length digs
                   replicateStr amount str = concat $ replicate amount str
                   resolveRoman xs l = 
				        if (l < 0) then ""
						else (getNextLiteral (head xs) (10^l)) ++ (resolveRoman (drop 1 xs) (l-1))
							 where getNextLiteral x pwr = 
								if (x < 1) then ""
								else if (x < 4) then replicateStr x (romans ! (pwr))
								else if (x > 5 && x < 9) then (romans ! (5*pwr)) ++ replicateStr (x-5) (romans ! (pwr))
								else romans ! (x*pwr)
								
								
arabics :: Map Char Int
arabics = fromList [('I', 1), ('V', 5), ('X', 10), ('L', 50), ('C', 100), ('D', 500), ('M', 1000)]
								
romanToNum :: [Char] -> Int
romanToNum [] = 0
romanToNum (x:[]) = arabics ! x
romanToNum xs = let fst = arabics ! (xs !! 0)
                    snd = arabics ! (xs !! 1)
                    tl = romanToNum (tail xs)
                in if (fst < snd) then tl - fst else tl + fst