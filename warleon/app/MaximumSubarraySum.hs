module MaximumSubarraySum where

import Data.List

linearSum :: (Foldable t, Num a) => t a -> [a]
linearSum = tail . reverse . foldl (\a x -> (x+ head a):a) [0] 

sureIndex :: Eq a => a -> [a] -> Int
sureIndex x xs =
  case elemIndex x xs of
    Just i -> i

maxLinearSum :: (Ord c, Num c) => [c] -> (Int,c)
maxLinearSum xs = (sureIndex m arr, m)
  where
    arr = linearSum xs
    m = maximum arr

findMaxCrossingSubArray :: (Ord c, Num c) => [c] -> [c] -> (Int,Int,c)
findMaxCrossingSubArray xs ys = ((length xs)-li-1,ri,ls+rs)
  where
    (li,ls) = maxLinearSum $ reverse xs
    (ri,rs) = maxLinearSum ys

findMaximumSubArray :: (Ord c, Num c) => [c] -> (Int,Int,c)
findMaximumSubArray [x] = (0,0,x)
findMaximumSubArray xs
  |lsum >= rsum && lsum >= csum = (lli,lhi,lsum)
  |rsum >= lsum && rsum >= csum = (rli,rhi,rsum)
  |otherwise = (cli,(length ls) + chi,csum)
  where
    (ls,rs)= splitAt (div (length xs) 2) xs
    (lli,lhi,lsum) = findMaximumSubArray ls
    (rli,rhi,rsum) = findMaximumSubArray rs
    (cli,chi,csum) = findMaxCrossingSubArray ls rs