module Goedel
where
import PR
import Cantor

eList :: [Integer] -> Integer
eList [] = pair 0 0
eList (x:xs) = pair 1 (pair x (eList xs))

dList :: Integer -> [Integer]
dList l = let (t,c) = unpair l
              (h, tl) = unpair c
          in case t of
             0 -> []
             1 -> h:(dList tl)
             _ -> []  -- make it total; nonsense is nil

ePR :: PrimRec -> Integer
ePR Z = pair 0 0
ePR S = pair 1 0
ePR (P i) = pair 2 (toInteger i)
ePR (C f gs) = pair 3 (pair (ePR f) (eList (map ePR gs)))
ePR (PR g h) = pair 4 (pair (ePR g) (ePR h))


dPR x = let (t,b) = unpair x
            (b1,b2) = unpair b -- note:  Lazy
        in case t of
           0 -> Z
           1 -> S
           2 -> if b == 0 then P 1 else P (fromInteger b)
           3 -> C (dPR b1) (map dPR (dList b2))
           4 -> PR (dPR b1) (dPR b2)
           _ -> Z

unpack 0 _ = []
unpack 1 x = [x]
unpack n x = (a:unpack (n-1) b) where (a,b) = unpair x

diag x = (eval p (unpack (arity p) x)) + 1
   where p = dPR x

diag_abs f x = (f p (unpack (arity p) x))
   where p = dPR x

diagonal x = (eval p (take (arity p) (repeat x)))
   where p = dPR x

notdiagonal x = 1 + diagonal x

evalGoedel i x = eval p (take (arity p) (repeat x))
    where p = dPR i

abs_diagonal f x = (f p (take (arity p) (repeat x)))
   where p = dPR x

---------------------------------------------
ePRstring :: PrimRec -> String
ePRstring Z = pairS "0" "0"
ePRstring S = pairS "1" "0"
ePRstring (P i) = pairS "2" (show (toInteger i))
ePRstring (C f gs) = pairS "3" (pairS (ePRstring f) (eListString (map ePR gs)))
ePRstring (PR g h) = pairS "4" (pairS (ePRstring g) (ePRstring h))

pairS x y = "("++x++","++y++")"

eListString :: [Integer] -> String
eListString [] = pairS "0" "0"
eListString (x:xs) = pairS "1" (pairS (show x) (eListString xs))



