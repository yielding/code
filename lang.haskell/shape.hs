data Shape = Point | Rectangle Double Double | Circle Double

area Point = 0
area (Rectangle width height) = width * height
area (Circle radius) = 2 * pi * radius

main = do
  print $ area(Rectangle 10 10)
  print $ area(Circle 20)
  print $ area(Point)