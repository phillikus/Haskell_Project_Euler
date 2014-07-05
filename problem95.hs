import Data.List (maximumBy)
import Data.Function (on)

divisors n 0 = 0
divisors n t = if ((n `mod` t) == 0) then t + divisors n (t-1) else (divisors n (t-1))

sumDivisors 0 = 0
sumDivisors n = divisors n (n-1)

amicChain n = let amic x = sumDivisors x
                  f xs x = if (x > 999999) then []
                           else if (x `elem` xs) then xs
                           else f (xs ++ [x]) (amic x)
              in f [] n

problem95 = minimum $ maximumBy (compare `on` length) $ map amicChain [1..999999]
