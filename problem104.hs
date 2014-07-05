import Data.List
import Data.Char
 
fibos = rec 0 1
    where
        rec a b = a:rec b (a+b)
 
fibo_2nk n k = 
    let        
        fkm1 = fibo (k-1)
        fkm2 = fibo (k-2)
        fk = fkm1 + fkm2
        fnp1 = fibo (n+1)
        fnp1sq = fnp1^2
        fn = fibo n
        fnsq = fn^2
    in
        fk*fnp1sq + 2*fkm1*fnp1*fn + fkm2*fnsq
 
fibo x = 
    let
        threshold = 30000
        n = div x 3
        k = n+mod x 3
    in
        if x < threshold 
        then fibos !! x
        else fibo_2nk n k
 
findCandidates = rec 0 1 0
    where
        m = 10^9
        rec a b n  =
            let
                continue = rec b (mod (a+b) m) (n+1)
                isBackPan a = (sort $ show a) == "123456789"
            in
                if isBackPan a 
                then n:continue
                else continue
search = 
    let
        isFrontPan x = (sort $ take 9 $ show x) == "123456789"
    in
        map fst
            $ take 1
            $ dropWhile (not.snd)            
            $ zip findCandidates
            $ map (isFrontPan.fibo) findCandidates
 
problem_104 = search