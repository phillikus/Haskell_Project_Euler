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

phi 1 = 1
phi n = numerator ratio `div` denominator ratio
 where ratio = foldl (\acc x -> acc * (1 - (1 % x))) 
                 (n % 1) $ nub (primeFactors n)

problem69 = let phiDivN n = (fromIntegral n) / (fromIntegral (phi n))
                maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]
            in 1 + (maxIndex $ map phiDivN [1..1000000])
