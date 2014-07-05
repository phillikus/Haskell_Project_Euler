import Data.List

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

primes :: [Integer]
primes = sieve (2 : 3 : possible [1..]) where
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
     possible (x:xs) = 6*x-1 : 6*x+1 : possible xs
	 
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise = factor n ps
		
consecutivePrimes n = let f x = (length $ rmdups $ primeFactors x) == 4
                        in if ((f n) && (f (n+1)) && (f (n+2)) && (f (n+3))) then [n,(n+1),(n+2),(n+3)] else consecutivePrimes (n+1)
						
main = print $ head $ consecutivePrimes 1