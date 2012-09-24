fibs::Integral i => [i]
--fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
fibs = 1: scanl (+) 2 fibs

stop = 4000000
total = sum $ filter even $ takeWhile (< stop) fibs
