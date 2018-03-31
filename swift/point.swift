#!/usr/bin/env swift

struct Point {
  var x = 0.0, y = 0.0

  mutating func moveByX(_ deltaX: Double, y deltaY: Double) {
    x += deltaX
    y += deltaY
  }
}

var somePoint = Point(x: 1.0, y: 1.0)
somePoint.moveByX(2.0, y: 3.0)

print("The point is now at (\(somePoint.x), \(somePoint.y))")
