isInt x = x == fromInteger (round x)

triangles = let f n =  n*(n+1) / 2
            in map f [1..]	
			
isPentagonal x = isInt $ (sqrt (24 * x + 1) + 1) / 6

isHexagonal x = isInt $ (sqrt (8 * x + 1) + 1) / 4

isHexAndPent x = isPentagonal x && isHexagonal x

main = print $ head $ drop 2 $ filter (isHexAndPent) triangles