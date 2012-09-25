module Euler (
  factors,
  divides,
  primes,
) where

import Data.List (find)

divides :: Integral a => a -> a -> Bool
d `divides`  k = k `mod` d == 0

factors :: Integral i => i -> [i]
factors n = let
    factors' _ 1 = []
    factors' start i = case find (`divides` i) [start..maxPossible] of
      Nothing -> [i]
      Just d  -> d : factors' d (i `div` d)
      where
        maxPossible = floor $ sqrt (fromIntegral i)
  in factors' 2 n

isPrime :: Integral i => i -> Bool
isPrime i = head (factors i) == i

primes = 2 : (filter isPrime [3, 5 ..])
