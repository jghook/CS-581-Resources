module TraceMuR
where
import Prelude hiding (pred,and,or,not)
import Data.Maybe (mapMaybe,isJust)
import Control.Monad (guard)
import Data.List (inits)
import MuR
import MuRCantor

third (_,_,c) = c
result = third . last

trace :: MuR -> [Integer] -> [(MuR,[Integer],Integer)]
trace Z xs = [(Z,xs,0)]
trace S (x:xs) = [(S,(x:xs),x+1)]
trace S [] = [(S,[],0)]
trace (P n) xs = [(P n, xs, nth n xs)]
trace (C f gs) xs = let gtraces = map (\g -> trace g xs) gs
                        ys = map result gtraces
                        ftrace = trace f ys
                    in (concat gtraces) ++ ftrace ++ [(C f gs, xs, result ftrace)]
trace (PR g h) (0:xs) = let gtrace = trace g xs
                          in gtrace++[(PR g h,0:xs, result gtrace)]
trace (PR g h) (x:xs) = let rectrace = trace (PR g h) ((x-1):xs)
                            r = result rectrace
                htrace = trace h ((x-1):r:xs)
            in rectrace ++ htrace ++ [(PR g h, x:xs, result htrace)]
trace (PR g h) [] = [(PR g h, [], 0)]
trace (Mu f) (xs) = trace_from f xs 0 []

trace_from f xs n tr = let fntr = trace f (n:xs)
                           r = result fntr
                       in case r of
                             0 -> tr ++ fntr ++ [(Mu f,xs,n)]
                 _ -> trace_from f xs (n+1) (tr ++ fntr)

find_trace ::  MuR -> [Integer] -> [(MuR,[Integer],Integer)] -> Maybe Integer
find_trace p xs [] = Nothing
find_trace p xs ((p',xs',y):ts) 
       | p == p' && xs == xs' = Just y
       | otherwise = find_trace p xs ts

verify_step :: [(MuR,[Integer],Integer)] -> (MuR,[Integer],Integer) -> Maybe ()
verify_step _ (Z,_,0)      = Just ()
verify_step _ (Z,_,_)      = Nothing
verify_step _ (S,(x:_),z)  = guard $ z == x + 1
verify_step _ (S,_,_)      = Nothing
verify_step _ (P i,xs,z)   = guard $ z == nth i xs
verify_step facts (C f gs,xs,z) 
        = do ys <- mapM (\ g -> find_trace g xs facts) gs
                 z' <- find_trace f ys facts 
                 guard $ z == z'
verify_step facts (PR g h,(0:xs),z) 
            = do z' <- find_trace g xs facts 
             guard $ z == z'
verify_step facts (PR g h,(x:xs),z) 
            = do r  <- find_trace (PR g h) (x-1:xs) facts
             r' <- find_trace h (x-1:r:xs) facts 
             guard $ z == r'
verify_step facts (Mu f,xs,z)
            = do mapM_ (verify_nonzero facts f xs) [0 .. (z - 1)]
             r <- find_trace f (z:xs) facts
                 guard $ r == 0
verify_step _ (PR _ _,[],z) = guard $ z == 0

verify_nonzero facts f xs n 
           = do r <- find_trace f (n:xs) facts
                guard $ r /= 0


verify :: [(MuR,[Integer],Integer)] -> Bool
verify tr = verify' [] tr where
   verify' facts [] = True
   verify' facts (t:ts) = isJust (verify_step facts t) && verify' (t:facts) ts

valid :: (MuR, [Integer], Integer) -> [(MuR, [Integer], Integer)] -> Bool
valid result trace = verify trace && (last trace == result)








