#!/usr/bin/env swift

func sumNumbers(_ numbers: Int...) -> Int {
  var total = 0
  for number in numbers {
    total += number
  }

  return total
}

let sum = sumNumbers(2, 3, 4, 5)
print(sum)
