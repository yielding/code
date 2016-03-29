#!/usr/bin/env swift

struct Resolution {
  var width  = 0
  var height = 0
  init(width: Int = 0, height: Int = 0) {
    self.width  = width
    self.height = height
  }
}

class VideoMode {
  var resolution = Resolution()
  var interlaced = false
  var frameRate  = 0.0
  var name: String?
}

let vga = Resolution(width: 640, height: 480)

struct FixedLengthRange {
  var firstValue: Int
  var length: Int
}

var rangeOfItems = FixedLengthRange(firstValue: 0, length: 3)
print(rangeOfItems.firstValue)
rangeOfItems.length = 4

print(rangeOfItems)

struct Point {
  var x = 0.0, y = 0.0
}

struct Size {
  var width = 0.0, height = 0.0
}

struct Rect {
  var origin = Point()
  var size = Size()
  var center: Point {
    get {
      let cx = origin.x + (size.width / 2)
      let cy = origin.y + (size.height / 2)
      return Point(x: cx, y: cy)
    }

    set (newCenter) {
      origin.x = newCenter.x - (size.width / 2)
      origin.y = newCenter.y - (size.height / 2)
    }
  }
}

var square = Rect(origin: Point(x: 0.0, y: 0.0)
                  size: Size(width:10.0, height: 10.0))
