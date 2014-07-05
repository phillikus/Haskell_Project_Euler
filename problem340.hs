digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

crazy a b c = let f n = if n > b then n - c else f(a + f(a + f(a + f(a + n))))
          in sum $ map f [0..b]
		  
problem340 = let digits = digs $ crazy (21^7) (7^21) (12^7)
             in drop ((length digits) - 9) digits