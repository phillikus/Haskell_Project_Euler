import Data.Set (fromList, member)

divisors :: Int -> [Int]
divisors n = [x | x<-[1..n-1], n `mod` x == 0]

abundants :: [Int]
abundants = let f 28123 = [28123]
                f n = if (sum $ divisors n) > n then n : f (n+1) else f (n+1)
            in f 12
		
abSum = fromList $ [a+b | a <- abundants,
                          b <- takeWhile (<=min a (28123-a)) abundants]
						  
main = print $ sum . filter (not . (`member` abSum)) $ [1..28123]