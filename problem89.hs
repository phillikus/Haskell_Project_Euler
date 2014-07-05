import NumberConverter
import System.IO  
import Control.Monad
				  
charsSaved x = let num = romanToNum x
                  in if (num > 3999 ) then 0
				  else (length x) - length (numToRoman num)

main = do 
        contents <- readFile "romans_long.txt"
        let lns = lines contents
		in print . sum $ map charsSaved lns