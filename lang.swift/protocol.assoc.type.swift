#!/usr/bin/env swift

protocol Container {
  associatedtype ItemType
  mutating func append(_ item: ItemType)

  var count: Int { get }
  subscript(i: Int) -> ItemType { get }
}

struct IntStack: Container {
  var items = [Int]()

  mutating func push(item: Int) {
    items.append(item)
  }

  mutating func pop() -> Int {
    return items.removeLast()
  }

  typealias ItemType = Int
  mutating func append(_ item: ItemType) {
    self.push(item: item)
  }

  var count: Int { return items.count }

  subscript(i: Int) -> Int {
    return items[i]
  }
}

struct Stack<T>: Container {
  var items = [T]()
  mutating func push(item: T) {
    items.append(item)
  }

  mutating func pop() -> T {
    return items.removeLast()
  }

  mutating func append(_ item: T) {
    self.push(item: item)
  }

  var count: Int {
    return items.count
  }

  subscript(i: Int) -> T {
    return items[i]
  }
}

extension Array: Container {}


func allItemsMatch<C1: Container, C2: Container
    where C1.ItemType == C2.ItemType,
          C1.ItemType: Equatable>
    (someContainer: C1, _ anotherContainer: C2) -> Bool {
  if someContainer.count != anotherContainer.count {
    return false
  }

  for i in 0..<someContainer.count {
    if someContainer[i] != anotherContainer[i] {
      return false
    }
  }

  return true
}

var s = Stack<Double>()
s.append(1.2)
s.append(2.3)
print(s.pop())
