quicksort [] = []
quicksort (x:xs) =
   let smallerSorted = quicksort (filter (<= x) xs)
       biggerSorted = quicksort (filter (>x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

getValue xs = quicksort $ digs xs

smallesMultiple = let f x = let cmp = getValue (2*x) 
                            in if (cmp == getValue (3*x) && 
							       cmp == getValue(4*x) && 
								   cmp == getValue(5*x) && 
								   cmp == getValue(6*x))
								then x
								else f (x+1)
				  in f 1