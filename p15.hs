
oldPaths n = numPaths' (0, 0)
  where
    numPaths' pt@(x,y) = if x == n || y == n then 1 else numPaths' (x+1, y) + numPaths' (x, y+1)

pathsList = 1:2:pathsList' 2
  where
    pathsList' n = (2*numPaths n (1,0)) : pathsList' (n+1)
    numPaths n (x,y)
      |x == y           = pathsList!! (n-x)
      |x == n || y == n = 1
      |otherwise        = numPaths n (x+1, y) + numPaths n (x, y+1)

paths = (pathsList!!)
