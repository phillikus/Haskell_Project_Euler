divisors n 0 = 0
divisors n t = if ((n `mod` t) == 0) then t + divisors n (t-1) else (divisors n (t-1))

sumDivisors 0 = 0
sumDivisors n = divisors n (n-1)

main = let amicable n = if sumDivisors n == n then 0 else if n == sumDivisors (sumDivisors n) then n else 0
       in print sum map amicable [1..10000]