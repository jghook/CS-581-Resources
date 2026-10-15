module H9
       where
import Prelude hiding (pred,and,or,not,sqrt,div)
import PR
import Cantor
import Goedel
import Trace
import Examples

aa = eval (P 2) [1,2,3]
ab = eval Z [21, 17, 42]
ac = eval S [13]
ad = eval (C S [Z]) [1,2,3]
ae = eval (PR Z Z) [3]
af = eval (PR Z (C S [P 1])) [3]

ba = pair 5 6
bb = pair 0 12
bc = unpair 22

ea = trace2 (PR (C S [Z]) Z) [0]
eb = trace2 (PR (C S [Z]) Z) [1]
ec = trace2 (PR Z (C S [Z])) [0]
ed = trace2 (PR Z (C S [Z])) [1]
ee = trace2 (PR (P 2) (P 3)) [1,2,3]
