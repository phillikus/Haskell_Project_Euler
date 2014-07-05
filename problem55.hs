fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d
 
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

isPalindrome x = (fromDigits $ reverse $ digs x) == x

isLychrel x = let f x n = let rev = fromDigits $ reverse $ digs x 
                          in if (n > 50) then True
                             else if (isPalindrome (x + rev)) then False
							 else f (x + rev) (n+1)
						  in f x 0
						  
main = print $ length $ filter (isLychrel) [1..9999]