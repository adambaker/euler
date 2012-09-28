import Euler

numDivisors i
  |maxI * maxI == i = num - 1
  |otherwise        = num
    where
      num  = 2 * (length $ filter (`divides` i) [1..maxI])
      maxI = floor $ sqrt (fromIntegral i)

triangle = tail $ scanl (+) 0 [1..]

main = print . head . dropWhile (\i->numDivisors i < 500) $ triangle
