main = do
  s <- readFile "text/p13.txt"
  print . (take 10) . show . sum $ map ((read::String->Int) . (take 12)) (lines s)
