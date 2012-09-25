import Euler
main = putStrLn $ show $ sum (takeWhile (< 2*(10^6)) primes)
