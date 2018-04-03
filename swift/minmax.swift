#!/usr/bin/env swift

func minMax(array: [Int]) -> (min: Int, max: Int) {
  var cmin = array[0]
  var cmax = array[0]

  for val in array[1..<array.count] {
    if val < cmin {
      cmin = val
    } else if val > cmax {
      cmax = val
    }
  }

  return (cmin, cmax)
}

let arr: [Int] = [3, 5, 1, 2]
print(minMax(array: arr))
