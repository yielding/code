#!/usr/bin/env swift

struct Point {
  var x = 0.0, y = 0.0

  mutating func moveByX(deltaX: Double, y deltaY: Double) {
    self = Point(x: x + deltaX, y: y + deltaY)
  }
}

var fiexedPoint = Point(x: 3.0, y: 3.0)
fiexedPoint.moveByX(2.0, y: 3.0)

enum TriStateSwitch {
  case Off, Low, High

  mutating func next() {
    switch self {
      case Off : self = Low
      case Low : self = High
      case High: self = Off
    }
  }
}

var ovenLight = TriStateSwitch.Low

ovenLight.next()
ovenLight.next()
