module MuRGoedel
where
import Goedel (dList,eList)
import MuR
import Cantor

eMuR :: MuR -> Integer
eMuR Z = pair 0 0
eMuR S = pair 1 0
eMuR (P i) = pair 2 (toInteger i)
eMuR (C f gs) = pair 3 (pair (eMuR f) (eList (map eMuR gs)))
eMuR (PR g h) = pair 4 (pair (eMuR g) (eMuR h))
eMuR (Mu f) = pair 5 (eMuR f)

dMuR x = let (t,b) = unpair x
             (b1,b2) = unpair b -- note:  Lazy
        in case t of
            0 -> Z
			1 -> S
			2 -> if b == 0 then P 1 else P (fromInteger b)
			3 -> C (dMuR b1) (map dMuR (dList b2))
			4 -> PR (dMuR b1) (dMuR b2)
			5 -> Mu (dMuR b)
			_ -> Z

loop = loop

halt :: MuR -> [Integer] -> Bool
halt Z _ = True
halt S _ = True
halt (P _) _ = True
halt (Mu S) _ = False
halt _ _ = undefined

notdiagonal x = if halt (dMuR x) (repeat x) then loop else 0


