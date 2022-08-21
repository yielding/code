-- double
double x = 2 * x
quadruple x = double (double x)

-- area
area r = pi * r ^ 2

-- heron
heron a b c = sqrt (s * (s - a) * (s - b) * (s -c))
    where
      s = (a + b + c) / 2

areaTriangleHeron a b c = result
  where
    result = sqrt (s * (s - a) * (s - b) * (s -c))
    s      = (a + b + c) / 2

-- guard
absolute x
  | x < 0     = -x
  | otherwise = x

noOfSolutions a b c
  | dis > 0  = 2
  | dis == 0 = 1
  | otherwise = 0
    where
      dis = b ^ 2 - 4*a*c

mySigSum x = 
  if x < 0
    then -1
    else if x > 0
      then 1
      else 0
