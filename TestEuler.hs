import Test.QuickCheck
import Euler

prop_product_of_factors :: Int -> Property
prop_product_of_factors i = (i > 1) ==> foldr (*) 1 (factors i) == i

main = quickCheck prop_product_of_factors
