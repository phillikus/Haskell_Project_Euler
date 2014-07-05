fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main = print $ length (takeWhile (<1000) $ map length $ map show fibs) + 1