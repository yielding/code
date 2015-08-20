#!/usr/bin/env swift

func findStringIndex(arr: [String], _ valueToFind: String) -> Int? {
    for (index, value) in arr.enumerate() {
        if value == valueToFind {
            return index
        }
    }

    return nil
}

let strs = ["cat", "doc", "llama", "parakeet", "terrapin"]

if let foundIndex = findStringIndex(strs, "llama") {
    print("The index of llama is \(foundIndex)")
}


func findIndex<T: Equatable>(arr: [T], _ valueToFind: T) -> Int? {
    for (index, value) in arr.enumerate() {
        if value == valueToFind {
            return index
        }
    }

    return nil
}

if let foundIndex = findIndex(strs, "llama") {
    print("The index of llama is \(foundIndex)")
}

let doubleIndex = findIndex([3.14, 0.1, 0.25], 9.3)
print(doubleIndex)

