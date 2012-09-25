import Euler

maxFactor i = last $ factors i
main = putStrLn $ show (maxFactor 600851475143)
