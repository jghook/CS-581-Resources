module PR
where
import Prelude hiding (pred,and,or,not,sqrt,div)
import Data.Maybe (mapMaybe,isJust)
import Control.Monad (guard)
import Data.List (inits)

data PrimRec 
        = Z
        | S    
        | P Int
        | C PrimRec [PrimRec]
        | PR PrimRec PrimRec
  deriving (Show, Eq)

eval :: PrimRec -> [Integer] -> Integer
eval Z _ = 0
eval S (x:_) = x+1
eval S _  = 0 -- default value for erroneous case
eval (P n) xs | n <= length xs = nth n xs
eval (P n) xs = 0 -- default value for erroneous case
eval (C f gs) xs = eval f (map (\g -> eval g xs) gs)
eval (PR g h) (x:xs) = 
   if x==0 then eval g xs
           else eval h ((x-1) : eval (PR g h) ((x-1):xs) : xs)
eval (PR _ _) [] = 0  -- default value for erroneous case

nth _ []    = 0 -- default value for erroneous case
nth 0 _     = 0 -- default value for erroneous case
nth 1 (x:_) = x
nth (n) (_:xs) = nth (n-1) xs

check :: PrimRec -> Int -> Bool
check Z _ = True
check S 1 = True
check (P n) m = n <= m
check (C f gs) n = check f (length gs) && (all (\g -> check g n) gs)
check (PR g h) n = check g (n-1) && check h (n+1)

arity :: PrimRec -> Int
arity Z = 0
arity S = 1
arity (P i) = i
arity (C f []) = 0
arity (C f gs) = maximum (map arity gs)
arity (PR g h) = (arity g + 1) `max` (arity h -1)

