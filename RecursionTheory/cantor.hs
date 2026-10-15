module Cantor
where

--  Square root scraped from the URL:
--  http://www.haskell.org/haskellwiki/Generic_number_type

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

pair :: Integer -> Integer -> Integer
pair k1 k2 = ((k1 + k2) * (k1 + k2 +1) `div` 2) + k2

unpair :: Integer -> (Integer,Integer)
unpair z = let w = (squareRoot (8*z + 1) - 1) `div` 2 
               t = (w * w + w) `div` 2
               y = z - t
               x = w - y
           in (x, y)

pi1 = fst . unpair
pi2 = snd . unpair

