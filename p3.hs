import Euler
import Data.List (find)
import Test.QuickCheck

factors :: Integral i => i -> [i]
factors n = let
    factors' _ 1 = []
    factors' start i = case find (`divides` i) [start..maxPossible] of
      Nothing -> [i]
      Just d  -> d : factors' d (i `div` d)
      where
        maxPossible = floor $ sqrt (fromIntegral i)
  in factors' 2 n

prop_product_of_factors :: Int -> Property
prop_product_of_factors i = (i > 1) ==> foldl (*) 1 (factors i) == i

maxFactor i = last $ factors i

main = putStrLn $ show (maxFactor 600851475143)
