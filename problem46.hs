primes :: [Integer]
primes = sieve (2 : 3 : possible [1..]) where
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
     possible (x:xs) = 6*x-1 : 6*x+1 : possible xs

isPrime :: Integer -> Bool
isPrime n = shortCircuit || (not $ any divisible $ takeWhile inRangeOf primes) where
    shortCircuit = elem n [2,3] || (n < 25 && ((n-1) `mod` 6 == 0 || (n+1) `mod` 6 == 0))
    divisible y = n `mod` y == 0
    inRangeOf y = y * y <= n
	
oddComposites = filter (not . isPrime) [1,3..]

isSum n = let f p x = let gb = p + 2 * x^2
                      in if ((not . isPrime) p) then f (p+1) x 
                         else if (gb == n) then True
                         else if ((gb > n) && (p > n)) then False
                         else if ((gb > n) && (p < n)) then f (p+1) 1			  
                         else f p (x+1)
          in f 2 1
		  
main = print $ head $ dropWhile (isSum) oddComposites