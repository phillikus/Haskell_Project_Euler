import Data.List

toIntList :: [String] -> [Integer]
toIntList = map read
	
isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `rem` d == 0 = False
      | otherwise      = go (d+1)

primes = filter isPrime [2 .. ]

getCirculars :: [a] -> [[a]]
getCirculars = (\xs -> let n = length xs in (tail . take n . map (take n) . tails . cycle) xs)

isCircularPrime :: Integer -> Bool
isCircularPrime x = (all (isPrime) $ toIntList $ getCirculars (show x))

main = print $ length $ filter (isCircularPrime) $ takeWhile (<1000000) primes