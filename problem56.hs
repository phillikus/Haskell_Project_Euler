digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

maxSum :: Integer
maxSum = let f x = maximum $ map sum $ map digs $ map (^x) [1..99]
         in maximum $ map f [1..99]
		 
main = print maxSum