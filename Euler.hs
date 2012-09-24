module Euler where

divides :: Integral a => a -> a -> Bool
d `divides`  k = k `mod` d == 0
