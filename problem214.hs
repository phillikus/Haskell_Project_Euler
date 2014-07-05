import Data.List (nub)
import Data.Ratio

primes = sieve (2 : 3 : possible [1..]) where
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
     possible (x:xs) = 6*x-1 : 6*x+1 : possible xs
	 
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
		
phi 1 = 1
phi n = if (isPrime n) then (n-1) else numerator ratio `div` denominator ratio
 where ratio = foldl (\acc x -> acc * (1 - (1 % x))) 
                 (n % 1) $ nub (primeFactors n)
				 
totients = map phi [0..40000000]
totient x = totients !! x
				 
totChain 1 = [1]
totChain n = [n] ++ (totChain (totient (fromInteger n)))

condition x = let xs = totChain x
              in ((length xs) == 25) 
			 
problem214 = sum $ filter (condition) $ takeWhile (<40000000) primes