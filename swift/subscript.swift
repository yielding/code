#!/usr/bin/env swift

struct TimeTable {
    let multiplier: Int

    subscript(index: Int) -> Int {
        return multiplier * index
    }
}

let threeTimesTable = TimeTable(multiplier: 3)
print("six times three is \(threeTimesTable[6])")
