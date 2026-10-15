module Trace 
where
import Prelude hiding (pred,and,or,not)
import Data.Maybe (mapMaybe,isJust)
import Control.Monad (guard)
import Data.List (inits)
import PR hiding (check)
import PRCantor

third (_,_,c) = c
result = third . last

--------------------------------------------
-- A trace tree

data TraceTree x = Ztree x
         | Stree x 
         | Ptree Int x
         | Ctree (TraceTree x) [TraceTree x] x
         | PRtree0 (TraceTree x) x
         | PRtreeN (TraceTree x) (TraceTree x) x
         | Stuck x
         deriving (Show, Eq)
         
tag (Ztree x) = x
tag (Stree x) = x
tag (Ptree n x) = x
tag (Ctree x xs y) = y
tag (PRtree0 y z) = z 
tag (PRtreeN x y z) = z 
tag (Stuck z) = z

res x = z where (_,_,z) = tag x         

----------------------------------------------
-- Generate a trace tree

trace2:: PrimRec -> [Integer] -> TraceTree (PrimRec,[Integer],Integer)
trace2 Z xs = Ztree (Z,xs,0)
trace2 S (x:xs) = Stree (S,(x:xs),x+1)
trace2 S [] = Stuck (S,[],0)
trace2 (P n) xs | n <= length xs = Ptree n (P n,xs,nth n xs)
trace2 (P n) xs = Stuck (P n,xs,0)
trace2 (C f gs) xs = let gtraces = map (\g -> trace2 g xs) gs
                         ys = map res gtraces
                         ftrace = trace2 f ys
                     in Ctree ftrace gtraces (C f gs,xs,res ftrace)
trace2 (PR g h) (0:xs) =
   let trace = trace2 g xs
   in PRtree0 trace (PR g h,0:xs,res trace)
trace2 (PR g h) (x:xs) = 
   let rectrace = trace2 (PR g h) ((x-1):xs)
       r = res rectrace
       htrace = trace2 h ((x-1):r:xs)
       r' = res htrace
   in PRtreeN rectrace htrace (PR g h,x:xs,r')
trace2 (PR g h) [] = Stuck (PR g h,[],0)

-----------------------------------------------------
-- Check that a trace tree is valid

check:: TraceTree (PrimRec,[Integer],Integer) -> PrimRec -> Maybe Integer
check (Ztree (Z,xs,0)) Z = Just 0
check (Stree (S,x:xs,y)) S = 
   if y == (x+1) then Just y else Nothing
check (Ptree n (P m,xs,i)) (P k) = 
   if (n==m) && (k==m) && (m <= length xs) && (nth m xs == i)
      then Just i
      else Nothing
check (Ctree t ts (C f gs,xs,i)) (C h ks) = 
  do { ys <- zipM check ts gs
     ; j <- check t f
     ; let (_,zs,k) = tag t
     ; if (zs == ys) && (j == i) && (k == i) && (h==f) && (gs==ks)
          then (Just i)
          else Nothing }
check (PRtree0 t (PR f g,0:xs,i)) (PR h j) = 
  do { z <- check t f
     ; if (z==i) && (f==h) && (g==j)
          then Just i 
          else Nothing }
check (PRtreeN s t (PR f g,n:xs,i)) (PR h j) =
  do { r <- check s (PR f g)
     ; k <- check t g
     ; let (_,ws,_) = tag s
           (_,zs,_) = tag t
     ; if (f==h) && (g==j) && (k==i) && (ws==(n-1):xs) && (zs==(n-1):r:xs)
          then (Just i)
          else Nothing }  
check x y = Nothing


test x xs = check (trace2 x xs) x

--------------------------------------------------------
-- helper functions

zipM:: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe[c]
zipM f [] [] = Just []
zipM f (x:xs) (z:zs) = do { y <- f x z; ys <- zipM f xs zs; return(y:ys)}


plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    

-------------------------------------------------------
-- Print a trace tree as indented lines

indent:: Int -> (PrimRec,[Integer],Integer) -> String
indent n (x,y,z) = take n (repeat ' ')++ plistf show (show x++" (") y "," (") = "++show z++"\n")

pp n (Ztree x) = indent n x
pp n (Stree x) = indent n x
pp n (Ptree b x) = indent n x
pp n (Ctree x xs y) = pp (n+3) x ++
                   concat (map (pp (n+3)) xs) ++
                   indent n y
pp n (PRtree0 y x) = pp (n+3) y ++ indent n x   
pp n (PRtreeN x y z) = pp (n+3) y ++ pp (n+3) x ++ indent n z
pp n (Stuck x) = "STUCK "++ indent n x

p1 = PR (P 1) (C S [P 2])  

--------------------------------------------------------
-- older versions

trace :: PrimRec -> [Integer] -> [(PrimRec,[Integer],Integer)]
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

find_trace ::  PrimRec -> [Integer] -> [(PrimRec,[Integer],Integer)] -> Maybe Integer
find_trace p xs [] = Nothing
find_trace p xs ((p',xs',y):ts) 
       | p == p' && xs == xs' = Just y
       | otherwise = find_trace p xs ts

verify_step :: [(PrimRec,[Integer],Integer)] -> (PrimRec,[Integer],Integer) -> Maybe ()
verify_step _ (Z,_,0)    = Just ()
verify_step _ (Z,_,_)    = Nothing
verify_step _ (S,[x],z)  = guard $ z == x + 1
verify_step _ (S,[],0)   = Just ()
verify_step _ (S,_,_)    = Nothing
verify_step _ (P i,xs,z) = guard $ z == nth i xs
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
verify_step _ (PR _ _,[],z) = guard $ z == 0

verify :: [(PrimRec,[Integer],Integer)] -> Bool
verify tr = verify' [] tr where
   verify' facts [] = True
   verify' facts (t:ts) = isJust (verify_step facts t) && verify' (t:facts) ts







