digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fromDigs = foldl addDig 0
   where addDig num d = 10*num + d

divisors :: Int -> [Int]
divisors n = [x | x<-[1..n], n `mod` x == 0]

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

isPrime :: Int -> Bool
isPrime n = if (n < 2) then False else
                if (length (divisors n)) == 2 then True else False

truncable n = let truncableR 0 = True
	          truncableR a = if isPrime a then truncableR $ fromDigs $ drop 1 $ digs a
              			     else False
                  truncableL 0 = True
                  truncableL a = if isPrime a then truncableL $ fromDigs $ take ((length $ digs a) - 1) $ digs a
				     else False
	      in (truncableR n) && (truncableL n)

truncableSum _ 0 = 0
truncableSum (x:xs) l = if truncable x then x + truncableSum xs (l-1) else truncableSum xs l

main = print $ truncableSum (drop 4 $ map fromInteger primes) 11