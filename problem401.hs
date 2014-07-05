divisors n = [x | x<-[1..n], n `mod` x == 0]

sigma2 n = sum $ map (^2) $ divisors n

sigma2To n = sum $ map sigma2 [1..n]

