import Data.Char
import Data.List

triangles :: (Integral x) => Int  -> [x]
triangles x = take x [ div (a^2 + a) 2   | a <- [1..]]

score :: String -> Int
score str = sum $ map (\a -> ord a - 64) str 

triword :: String -> Bool
triword str = elem y ( triangles y )
  where y = score str

main = do
  input <- readFile "words.txt"
  let wordlist = lines input
  let results = filter triword wordlist
  print ( length $ results )
  return ()