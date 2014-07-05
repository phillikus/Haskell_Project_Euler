fac 0 = 1
fac n = n * fac (n-1)

facs = map fac [0..]
facs' x = facs !! x

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

f n = sum $ map facs' $ digs n

f' n = let h x = last $ takeWhile(<x) facs
       in 

sf n = sum $ digs (f n)

sf' n = [(x,y,z) | x <- map facs' [0..1000], y <- map facs' [0..1000], z <- map facs' [0..1000], (x+y+z) == n]

sfs = map sf [0..]
sfs' x = sfs !! x

g i = let f x = if ((sfs' x) == i) then x else f(x+1)
      in f 488889
	  
sg i = sum $ digs (g i)