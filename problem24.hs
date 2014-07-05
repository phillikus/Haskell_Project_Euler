import Data.List
main = print $ (!! 999999) . sort $ permutations ['0'..'9']