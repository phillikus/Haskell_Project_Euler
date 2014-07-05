import EulerMath

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

r x = fromDigits $ replicate x 1

unique :: [Integer] -> [Integer]
unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]

main = print $ sum $ take 40 $ primeFactors (r (10^9))