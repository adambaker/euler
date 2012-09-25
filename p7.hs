import Euler

isPrime :: Integral i => i -> Bool
isPrime i = head (factors i) == i

primes = 2 : (filter isPrime [3, 5 ..])
