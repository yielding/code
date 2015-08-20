#!/usr/bin/evn swift

/// 
/// the variable 'runningTotal' is captured
///
func makeIncrementer(forIncment amount: Int) -> Void -> Int {
    var runningTotal = 0
    func incrementer() -> Int {
        runningTotal += amount
        return runningTotal
    }

    return incrementer
}

let incByTen = makeIncrementer(forIncment: 7)
let alsoIncByTen = incByTen

print(alsoIncByTen())
print(alsoIncByTen())
print(alsoIncByTen())
print(incByTen())

