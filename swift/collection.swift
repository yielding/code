#!/usr/bin/env swift

var someInts = [Int]()
someInts.append(1)
print(someInts.count)

var str: String = "이창하"
print(str.characters.count)

var doubles = [Double](count:3, repeatedValue: 0.0)
print(doubles)

var fg: Set<String> = ["Rock", "Classical", "Hip hop"]
print(fg)

func sayHello(personName: String, xx: Bool) -> String {
    let greeting = "Hello, " + personName + "!"

    return greeting
}

func arithmeticMean(numbers: Double...) -> Double {
    var total: Double = 0
    for no in numbers {
        total += no
    }

    return total / Double(numbers.count)
}

// print(arithmeticMean(1, 2, 3, 4, 5))

func alignRight(var string: String, totalLength: Int, pad: Character) -> String {
   let amountToPad = totalLength - string.characters.count
   if amountToPad < 1 {
       return string
   }

   let padString = String(pad)

   for _ in 1...amountToPad {
       string = padString + string
   }

   return string
}

let origitalString = "Hello"
let paddedString = alignRight(origitalString, totalLength: 10, pad: "-")
print(paddedString)
