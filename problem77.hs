primes = sieve (2 : 3 : possible [1..]) where
     sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
     possible (x:xs) = 6*x-1 : 6*x+1 : possible xs

ways [] = 1 : repeat 0
ways (x:xs) = n where n = zipWith (+) (ways xs) (replicate x 0 ++ n)
	 
problem77 = dropWhile (< 5) $ ways $ take 5000 primes