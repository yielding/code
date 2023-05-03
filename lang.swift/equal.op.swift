#!/usr/bin/env swift

class Point {
  var x, y: Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

  func swap(_ oth: inout Point) { 
    swapper(a: &self.x, b: &oth.x)
    swapper(a: &self.y, b: &oth.y)
  }

  var description: String {
    return "\(self.x, self.y)"
  }
}

func == (l: Point, r: Point) -> Bool {
  return l.x == r.x && l.y == r.y
}

func swapper<T>(a: inout T , b: inout T) {
  let tmp = a; a = b; b = tmp
}


var p = Point(x: 1, y: 2)
var q = Point(x: 10, y: 30)


p.swap(&q)

print("\(p.description)")
