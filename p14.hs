import Euler

chainLength 1 = 1
chainLength i
  |2`divides`i = 1 + chainLength (i`div`2)
  |otherwise   = 2 + chainLength((3*i+1)`div`2)

millonTo500k = map (\i->(chainLength i, i)) [499999,500001..(10^6-1)]

main = print . maximum $ millonTo500k
