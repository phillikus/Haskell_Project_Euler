import Data.List

toIntList :: [String] -> [Integer]
toIntList = map read

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

perms n = toIntList $ filter f $ permutations $ show n
          where f n = (n !! 0 /= '0')

isCubic n = (round ((fromInteger n) ** (1/3)) ^ 3) == n

cubes = map (^3) [1..]

permCubes = let f x = filter (isCubic) $ rmdups $ perms x
            in map f cubes
	
condition xs = (length xs) < 4
			  
result = head $ dropWhile (condition) permCubes
