module IsPrime where

sqrti :: Integer -> Integer
sqrti x = (ceiling . sqrt . fromIntegral) x

isPrime :: Integer -> Bool
isPrime x
  | x < 0 = False
  | x == 0 = False
  | x == 1 = False
  | otherwise = not $ any (\n -> (x `rem` n) == 0) [2..(sqrti x)]
