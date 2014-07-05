module EulerMath
( 
  quicksort,
  fac,
  fibs,
  digits, 
  divisors,
  primes,
  primeFactors,
  isPrime,
  toIntList,
  rmdups
) where

import Data.List

quicksort [] = []
quicksort (x:xs) =
   let smallerSorted = quicksort (filter (<= x) xs)
       biggerSorted = quicksort (filter (>x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

fac :: (Eq a, Num a) => a -> a
fac 0 = 1
fac n = n * fac (n-1)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

divisors :: Int -> [Int]
divisors n = [x | x<-[1..n-1], n `mod` x == 0]

primes :: [Integer]
primes = sieve (2 : 3 : possible [1..]) where
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
     possible (x:xs) = 6*x-1 : 6*x+1 : possible xs
	 
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise = factor n ps
		
isPrime :: Integer -> Bool
isPrime n = shortCircuit || (not $ any divisible $ takeWhile inRangeOf primes) where
    shortCircuit = elem n [2,3] || (n < 25 && ((n-1) `mod` 6 == 0 || (n+1) `mod` 6 == 0))
    divisible y = n `mod` y == 0
    inRangeOf y = y * y <= n

toIntList :: [String] -> [Integer]
toIntList = map read

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . quicksort