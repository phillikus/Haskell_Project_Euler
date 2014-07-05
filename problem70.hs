import Data.List (nub)
import Data.Ratio

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   let smallerSorted = quicksort (filter (<= x) xs)
       biggerSorted = quicksort (filter (>x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

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

totients = map phi [0..10000000]
totient x = totients !! x				 
				 
nDivPhi n = (fromIntegral n) / (fromIntegral (totient n))
	
isPerm n = equalDigs (phi n) n
           where equalDigs x y = (quicksort $ digs x) == (quicksort $ digs y)

problem70 = let minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]
            in 2 + (minIndex $ map nDivPhi [2..10000000])