module PRCantor
where
import Prelude hiding (pred,and,or,not,sqrt,div)
import Data.Maybe (mapMaybe,isJust)
import Control.Monad (guard)
import Data.List (inits)
import PR


-- Elementary arithmetic
mkconst 0 = Z
mkconst n = C S [mkconst (n-1)]

one    = mkconst 1
plus   = PR (P 1) (C S [P 2])
times  = PR Z (C plus [P 2, P 3])
pred   = PR Z (P 1)
monus' = PR (P 1) (C pred [P 2])
monus  = C monus' [P 2, P 1]
          

-- Elementary logic
false = Z
true  = one
and   = PR false (P 3)
or    = PR (P 1) true
not   = PR true false
ifte  = PR (P 2) (P 3)

-- Predicates
iszero  = PR true false
nonzero = PR false true
equal   = C and [C iszero [monus],
                 C iszero [monus']]
lteq    = C iszero [monus]
lt      = C nonzero [monus']

-- Bounded minimization (search)
bmin_model p (0:xs) = 1 
bmin_model p (x:xs) = let r = (bmin_model p (x-1:xs)) 
                      in if r < x then r
                         else if p (x-1:xs) then x-1 else x

bmin p n = PR one
              (C ifte [C lt [P 2, P 1], 
                       P 2, 
                       C ifte [C p (P 1: take (n-1) (map (\j -> P j) [3..])), 
                               P 1, 
                               C S [P 1]]])

-- Divisibility
isdiv_model q a b = (q * b <= a) && (a < (q+1) * b)
isdiv = C and [ C lteq [C times [P 1, P 3], P 2],
                C lt   [P 2, C times [C S [P 1], P 3]]]
div = C (bmin isdiv 3) [C S [P 1], P 1, P 2]

-- Square Root
issqrt_model r x = (r^2 <= x) && (x < (r+1)^2)
issqrt = C and [ C lteq [C times [P 1, P 1],P 2],
                 C lt   [P 2, C times [C S [P 1], C S [P 1]]]]
sqrt = C (bmin issqrt 2) [P 1, P 1]


-- Cantor Pairing Function and Projections
pair = C plus [C div [C times [C plus [P 1, P 2],
                               C S [C plus [P 1, P 2]]],
                      mkconst 2],
               P 2]

w = C div [C pred [C sqrt [C S [C times [mkconst 8, P 1]]]], mkconst 2]
t = C div [C plus [C times [w,w],w], mkconst 2]
pi2 = C monus [P 1,t]
pi1 = C monus [w,pi2]

