total::Int
total = sum $ filter (\x-> x `mod` 3 == 0 || x `mod` 5 == 0) [1..999]

main = putStrLn $ show total
