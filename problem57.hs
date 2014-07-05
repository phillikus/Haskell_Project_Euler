import Data.Ratio

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fraction n = let f x = if (x == 1) 
                     then 0.5 
					 else 1 / (2 + (f (x-1)))
               in 1 + (f n)

problem57 = let f x = 
                if (x > 1000) then 0
                else if ((length $ digs $ numerator $ fraction x) > (length $ digs $ denominator $ fraction x))
				then 1 + f (x+1) else f(x+1)
			in f 1
			