fac 0 = 1
fac n = n * fac (n-1)

c n r = (fac n) / ((fac r) * (fac (n-r)))

main = let f x = if (x > 100) then [] 
                              else (filter (> 1000000) $ map (c x) [0..x]) ++ f (x+1)
       in print $ length $ f 1