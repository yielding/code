
areaTriangleTrig a b c = c * height / 2
  where cosa   = (b^2 + c^2 - a^2) / (2*b*c)
        sina   = sqrt(1 - cosa^2)
        height = b*sina

areaTriangleHeron a b c = result
  where result = sqrt(s * (s - a) * (s - b) * (s - c))
        s      = (a + b + c) / 2

abs x
  | x < 0     = 0 - x
  | otherwize = x

noOfSolutions a b c
  | desc > 0  = 2
  | desc == 0 = 1
  | desc < 0  = 0
  where desc = b^2 - 4*a*c
