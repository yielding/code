#!/usr/bin/env swift

struct Countdown: Sequence, IteratorProtocol {
  var count: Int

  mutating func next() -> Int? {
    if count == 0 {
      return nil
    } else {
      defer { count -= 1 }
      return count
    }
  }
}

let threeToGo = Countdown(count: 3)

for i in threeToGo {
  print(i)
}
