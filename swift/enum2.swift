#!/usr/bin/env swift

enum CompassPoint:String {
  case North
  case South
  case East
  case West
}

enum Planet: Int {
  case Mercury = 1, Venus, Earth, Mars, Jupiter, 
  Saturn, Uranus, Neptune
}

var directionToHead = CompassPoint.South
switch directionToHead {
  case .North: print("n")
  case .South: print("s")
  case .East:  print("e")
  case .West:  print("w")
}

enum Barcode {
  case UPCA(Int, Int, Int, Int)
  case QRCode(String)
}

var pb0 = Barcode.UPCA(8, 85909, 51226, 3)
var pb1 = Barcode.QRCode("ABCDEFGHJI")

switch pb0 {
case .UPCA(let noSystem, let manufacturer, let product, let check):
  print("noSystem: \(noSystem)")
case .QRCode(let pCode):
  print("QR code: \(pCode)")
}

print(Planet(rawValue: 1))
