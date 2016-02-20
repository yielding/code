#!/usr/bin/env swift

class Point {
  var x, y: Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  func swap(inout oth: Point) { 
    swapper(&self.x, b: &oth.x)
    swapper(&self.y, b: &oth.y)
  }

  var description: String {
    return "\(self.x, self.y)"
  }
}

func == (l: Point, r: Point) -> Bool {
  return l.x == r.x && l.y == r.y
}

func swapper<T>(inout a: T , inout b: T) {
  let tmp = a; a = b; b = tmp
}


var p = Point(x: 1, y: 2)
var q = Point(x: 10, y: 30)


p.swap(&q)

print("\(p.description)")
