import Data.List
import Data.Ord

diophant x y d = (x^2 - d * y^2)


minSolution d = let f x y = if (di == 1) then x
                            else if (di > 1) then f x (y+1)
							else f (x+1) y
							where di = diophant x y d
			    in f 1 1
				
isSquare n = (round ((fromInteger n) ** (1/2)) ^ 2) == n
				
problem66 = fst $ maximumBy (comparing snd) xs
            where xs = [(x, minSolution x) | x <- filter (not . isSquare) [2..1000]]			      