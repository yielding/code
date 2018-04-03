#!/usr/bin/env swift

func arithmeticMean(_ numbers: Double...) -> Double {
  var total: Double = 0
  for no in numbers {
    total += no
  }

  return total / Double(numbers.count)
}

print(arithmeticMean(1, 2, 3, 4.0))
