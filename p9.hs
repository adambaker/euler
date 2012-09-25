import Euler
import Data.List (find)

triplets = [(a, b, 1000-a-b)| a <- [3..998], b <- [a..998]]
pythagorean = find (\(a, b, c)->a*a+b*b==c*c) triplets
pythagProduct = case pythagorean of Just (a, b, c) -> a*b*c

main = putStrLn $ show pythagProduct
