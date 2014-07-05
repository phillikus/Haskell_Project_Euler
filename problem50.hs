import Control.Monad

primes :: [Integer]
primes = sieve (2 : 3 : possible [1..]) where
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
     possible (x:xs) = 6*x-1 : 6*x+1 : possible xs

isPrime :: Integer -> Bool
isPrime n = shortCircuit || (not $ any divisible $ takeWhile inRangeOf primes) where
    shortCircuit = elem n [2,3] || (n < 25 && ((n-1) `mod` 6 == 0 || (n+1) `mod` 6 == 0))
    divisible y = n `mod` y == 0
    inRangeOf y = y * y <= n

findPrimeSum ps 
    | isPrime sumps = Just sumps
    | otherwise     = findPrimeSum (tail ps) `mplus` findPrimeSum (init ps)
    where
    sumps = sum ps
 
problem_50 = findPrimeSum $ take 546 primes