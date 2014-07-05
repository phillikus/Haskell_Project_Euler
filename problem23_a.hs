import Control.Applicative
import Data.List

divisors :: Int -> [Int]
divisors n = [x | x<-[1..n-1], n `mod` x == 0]

abundants :: [Int]
abundants = let f 28123 = [28123]
                f n = if (sum $ divisors n) > n then n : f (n+1) else f (n+1)
            in f 12
		
abundantSums = let f (x:xs) = (map (+x) $ dropWhile (<x) abundants) : f xs
		       in f abundants
