import Data.List

primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

quicksort [] = []
quicksort (x:xs) =
   let smallerSorted = quicksort (filter (<= x) xs)
       biggerSorted = quicksort (filter (>x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

isPandigital n = let digits = digs n
                     len = length $ digits
                 in if len > 9 then False
                 else (quicksort $ digits) == [1..len]
				 
panPrimes = filter (isPandigital) $ takeWhile (< 99999999) primes
