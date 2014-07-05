digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fac 0 = 1
fac n = n * fac(n-1)

curiosNumbers = let f 100000 = []
                    f n = if (sum $ map fac $ digs n) == n then n : f(n+1) else f(n+1)
                in f 3
				
main = print $ sum curiosNumbers