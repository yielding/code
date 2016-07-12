#!/usr/bin/env swift

// extension Array {
//   func each(f: (Element) -> Void) -> Array {
//     for e in self { f(e) }
// 
//     return self
//   }
// 
//   func eachWithIndex(f: (Element, Int) -> Void) -> Array {
//     var i = 0
//     for item in self { f(item, i); i += 1 }
// 
//     return self
//   }
// }
// 
// extension Range {
//   func each(f: (Element) -> Void) -> Range {
//     for e in self { f(e) }
// 
//     return self
//   }
// }
// 
// extension Dictionary {
//   func each(f: (Key, Value) -> Void) -> Dictionary {
//     for (key, value) in self {
//       f(key, value)
//     }
// 
//     return self
//   }
// }

extension Int {
  func times(block: () -> ()) {
    for _ in 0..<self { block() }
  }

  func times(block:(Int) -> ()) -> Int {
    for i in 0..<self { block(i) }
    return self
  }
}

3.times { print("hello") }
5.times { print("\($0)") }
// [1.1, 2.1, 3.1].each { print("\($0)") }
// [1:"kamin", 2:"gunhee", 3:"me"].each { print("\($0.0) => \($0.1)") }
// 
// var arr = [UInt16](2..<10)
// arr.each { print("\($0)") }
