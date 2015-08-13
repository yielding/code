#!/usr/bin/env swift

class NamedShape {
  var numberOfSides: Int = 0
  var name: String

  init(name: String) {
    self.name = name
  }

  func simpleDescription() -> String {
    return "A shape with \(numberOfSides) sides/"
  }
}

class Square: NamedShape {
  var sideLength: Double

  init(sideLength: Double, name: String) {
    self.sideLength = sideLength
    super.init(name: name)
    numberOfSides = 4
  }

  func area() -> Double {
    return sideLength * sideLength
  }

  override func simpleDescription() -> String {
    return "A square with \(sideLength) sides"
  }
}

class EquilateralTriangle: NamedShape {
  var sideLength: Double = 0.0

  init(sideLength: Double, name: String) {
    super.init(name: name)
    self.sideLength = sideLength
    numberOfSides = 3
  }

  var parameter: Double {
    get { return 3.0 * sideLength }
    set { sideLength = newValue / 3.0 }
  }

  override func simpleDescription() -> String {
    return "An equilateral triangle with sides of len \(sideLength)"
  }
}

class TriangleAndSqure {
    var triangle: EquilateralTriangle {
        willSet {
            square.sideLength = newValue.sideLength
        }
    }

    var square: Square {
        willSet {
            triangle.sideLength = newValue.sideLength
        }
    }

    init(size:Double, name:String) {
        square = Square(sideLength: size, name: name)
        triangle = EquilateralTriangle(sideLength: size, name: name)
    }
}

var triangleAndSqure = TriangleAndSqure(size:10, name: "anoter test shape")

print(triangleAndSqure.square.sideLength)
print(triangleAndSqure.triangle.sideLength)
triangleAndSqure.square = Square(sideLength: 50, name: "larger square")
print(triangleAndSqure.triangle.sideLength)

