import Data.Array as A
import Euler.List
import Euler

type Grid = A.Array (Int, Int)

gridIndexed :: [[a]] -> [(Int, [(Int, a)])]
gridIndexed grid = zip [1..] [zip [1..] line | line <- grid]

intGrid :: String -> [[Integer]]
intGrid = (map ((map read) . words)) . lines

gridBounds :: [[a]] -> ((Int, Int),(Int, Int))
gridBounds grid = ((1, 1), (maximum $ map length grid, length grid))

gridArray :: [[a]] -> Grid a
gridArray grid = A.array (gridBounds grid) [((x,y), val) | (y,line)<-(gridIndexed grid), (x, val)<-line]

upBound = snd . bounds

windowAcross :: Int -> Grid a -> [[a]]
windowAcross n grid =
  let nextN (i,j) = map (grid!) [(x, j) | x <- [i..i+n-1]]
  in map nextN [(i,j) | i <- [1..maxStartX], j <- [1..maxY]]
  where
    (maxX,maxY) = upBound grid
    maxStartX = maxX - n + 1

transpose :: Grid a -> Grid a
transpose grid = ixmap ((1,1), (maxX,maxY)) flipPair grid
  where
    flipPair (a,b) = (b,a)
    (maxY,maxX) = upBound grid

windowDown :: Int -> Grid a -> [[a]]
windowDown n = windowAcross n . Main.transpose

windowDiagDown :: Int -> Grid a -> [[a]]
windowDiagDown n grid =
  let nextN (i, j) = map (grid!) [(i+shift, j+shift) | shift <- [0..n-1]]
  in map nextN [(i,j) | i <- [1..maxStartX], j <- [1..maxStartY]]
  where
    (maxX,maxY) = upBound grid
    maxStartX = maxX - n + 1
    maxStartY = maxY - n + 1

windowDiagUp n grid = windowDiagDown n (reverseRows grid)
  where
    reverseRows grid = ixmap bounds' (\(x,y)->(x, newY y)) grid
    bounds' = bounds grid
    maxY = snd $ snd bounds'
    newY y = maxY - y + 1

products :: Num a => Grid a -> [a]
products grid = concatMap (\f->map product $ f 4 grid) [windowDown, windowAcross, windowDiagDown, windowDiagUp]

maxProduct grid = maximum $ map (\row->maximum $ map product (window 4 row)) grid

main = do
  s <- readFile "text/p11.txt"
  (print . maximum . products . gridArray . intGrid) s
