module Strassen where

import Data.List

instance Num a => Num [a] where
  (+) [] [] = []
  (+) (x:xs) (y:ys) = (x + y):(xs + ys)
  (-) [] [] = []
  (-) (x:xs) (y:ys) = (x - y):(xs - ys)

split :: [a] -> ([a],[a])
split xs = splitAt ((div (length xs) 2)) xs

splitMatrixH :: [[a]] -> [[[a]]]
splitMatrixH xss = [ass,bss]
  where
    (ass,bss) = split xss

splitMatrixV :: [[a]] -> [[[a]]]
splitMatrixV = map transpose . splitMatrixH . transpose

splitMatrix  :: [[a]] -> [[[a]]]
splitMatrix = foldr (++) [] . map splitMatrixV . splitMatrixH
 
mergeMatrixH :: [[a]] -> [[a]] -> [[a]]
mergeMatrixH xss yss = xss++yss

mergeMatrixV :: [[a]] -> [[a]] -> [[a]]
mergeMatrixV xss yss = transpose $ mergeMatrixH (transpose xss) (transpose yss)

strassen :: Num a => [[a]] -> [[a]] -> [[a]]
strassen [[x]] [[y]] = [[x*y]]
strassen xss yss = mergeMatrixH (mergeMatrixV c11 c12) (mergeMatrixV c21 c22)
  where
    [a11,a12,a21,a22] = splitMatrix xss
    [b11,b12,b21,b22] = splitMatrix yss
    s1 = b12 - b22 
    s2 = a11 + a12 
    s3 = a21 + a22 
    s4 = b21 - b11 
    s5 = a11 + a22 
    s6 = b11 + b22 
    s7 = a12 - a22 
    s8 = b21 + b22 
    s9 = a11 - a21 
    s10= b11 + b12 
    p1 = strassen a11 s1
    p2 = strassen s2 b22
    p3 = strassen s3 b11
    p4 = strassen a22 s4
    p5 = strassen s5 s6 
    p6 = strassen s7 s8 
    p7 = strassen s9 s10
    c11 = p5 + p4 - p2 + p6
    c12 = p1 + p2
    c21 = p3 + p4
    c22 = p5 + p1 - p3 - p7