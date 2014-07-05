ps = [] : map parts [1..]
    where parts n = [n] : [x : p | x <- [1..n], p <- ps !! (n - x), x <= head p]