import Euler

total::Int
total = sum $ filter (\x-> divides 3 x || divides 5 x) [1..999]

main = putStrLn $ show total
