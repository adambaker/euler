triangle :: String -> [[Int]]
triangle = (map ints) . lines
  where ints = (map read) . words

merge :: (Num a, Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y1:y2:ys) = x + (max y1 y2) : merge xs (y2:ys)
merge _ _ = []

reduceTriangle = foldr1 merge

main = readFile "text/p18.txt" >>= print . head . reduceTriangle . triangle
