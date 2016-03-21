#!/usr/bin/env swift

protocol ExampleProtocol {
  var simpleDescription: String { get }
  mutating func adjust()
}

class SimpleClass: ExampleProtocol {
  var simpleDescription: String = "A very simple class."

  var anotherProperty: Int = 69105
  func adjust() {
    simpleDescription += " Now 100% adjusted."
  }
}

var a = SimpleClass()
a.adjust()
let aDesc = a.simpleDescription
print(aDesc)

struct SimpleStructure: ExampleProtocol {
  var simpleDescription: String = "A simple structure"
  mutating func adjust() {
    simpleDescription += " (adjusted)"
  }
}

var b = SimpleStructure()
b.adjust()
let bDesc = b.simpleDescription
print(bDesc)
