module Karatsuba where

--import Data.Digits

add' :: Integral a => [a] -> [a] -> [a]
add' [] [] = []
add' (x:xs) [] = x:(add' xs [])
add' [] (x:xs) = x:(add' [] xs)
add' (x:xs) (y:ys) =
  let
    a = x + y
  in case (a>9) of
    True -> (mod a 10):(add' xs $ add' [1] ys)
    False -> a:(add' xs ys)

add :: Integral a => [a] -> [a] -> [a]
add xs ys = reverse $ add' (reverse xs) (reverse ys)

sub :: Integral a => [a] -> [a] -> [a]
sub xs ys = clear $ add xs $ map negate ys
  where
    clear :: (Integral a) => [a] -> [a]
    clear [] = []
    clear [x] = [x]
    clear (x:y:xs)
      | y<0 = (x-1):(clear $ (10+y):xs)
      | otherwise = x:(clear $ y:xs) 


split :: [a] -> ([a],[a])
split xs = splitAt ((div (length xs) 2)) xs

karatsuba :: [Integer] -> [Integer] -> [Integer]
karatsuba [] [] = []
karatsuba _ [] = []
karatsuba [] _ = []
karatsuba xs ys =
  (if null left then [0] else left)
  ++ (if null mid then [0] else mid)
  ++ (if null right then [0] else right)
  where
  (x1,x0) = split xs
  (y1,y0) = split ys
  left = karatsuba x1 y1
  right = karatsuba x0 y0
  p = karatsuba (add x1 x0) (add y1 y0)
  mid = sub p $ add left right

{-
karatsuba :: Integral a => a -> a -> a
karatsuba 0 0 = 0
karatsuba _ 0 = 0
karatsuba 0 _ = 0
karatsuba x y = left + mid + right
  where
    lx = length $ digits 10 x
    ly = length $ digits 10 y
    offx = floor $ (**) 10 $ fromIntegral $ div lx 2
    offy = floor $ (**) 10 $ fromIntegral $ div ly 2
    x1 = mod x offx 
    y1 = mod y offy
    x0 = div x offx 
    y0 = div y offy
    left = karatsuba x1 y1
    right = karatsuba x0 y0
    p = karatsuba (x1+x0) (y1+y0)
    mid = p - left - right
-}