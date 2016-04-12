#!/usr/bin/env swift

struct Stack<T> {
  var items = [T]()

  mutating func push(item: T) {
    items.append(item)
  }

  mutating func pop() -> T {
    return items.removeLast()
  }
}

extension Stack {
  var topItem: T? {
    return items.isEmpty ? nil : items[items.count - 1]
  }
}

var ss = Stack<String>()
ss.push("1")
ss.push("2")
ss.push("3")
ss.push("4")
ss.push("5")
ss.push("6")

print(ss.pop())

if let topItem = ss.topItem {
  print ("The top item on the stack is \(topItem).")
}
