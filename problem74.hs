import EulerMath

facs x = (map fac [0..9]) !! x

digitFacChain n = let digitFacSum x = sum $ map facs $ digits x
                      f xs x = if (x `elem` xs) then length xs
                               else f (x : xs) (digitFacSum x)
                  in f [] n

problem72 = length $ filter (==60) $ map digitFacChain [1..999999]
