isPalindrome :: Integral i => i -> Bool
isPalindrome n = let nStr = show n in
  nStr == reverse nStr

main = putStrLn $ show . maximum $ filter isPalindrome [x*y | x <- [1..999], y <- [1..999]]
