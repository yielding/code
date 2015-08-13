#!/usr/bin/env swift

func repeatItem<Item>(item: Item, noOfItems: Int) -> [Item] {
    var result = [Item]()
    for _ in 0..<noOfItems {
        result.append(item)
    }

    return result
}

// print(repeatItem("knoc", noOfItems: 4))

// enum OptionalValue<T> {
//     case None
//     case Some<T>
// }
// 
// var possibleInteger: OptionalValue<Int> = .None
// possibleInteger = .Some(100)
// 
// print(possibleInteger)

func anyCommonElements<T, U 
   where T: SequenceType,
         U: SequenceType,
         T.Generator.Element: Equatable,
         T.Generator.Element == U.Generator.Element>
         (lhs: T, _ rhs: U) -> Bool 
{
    for lhsItem in lhs {
        for rhsItem in rhs {
            if lhsItem == rhsItem { return true }
        }
    }

    return false
}

print(anyCommonElements([1, 2, 3], [3]))

let three = 3
let pointOneFour = 0.14159
let pi = Double(three) + pointOneFour

typealias AudioSample = UInt16
var maxAmp = AudioSample.min
let (a, b) = (1, 2)
print(b)

let possibleString: String? = "An optional string."
let forcedString  : String  = possibleString!
let assumedString : String! = "An implicitly unwrapped"
let implicitString: String  = assumedString

print("\u{E9}\u{20DD}")
print("\u{1F1FA}\u{1F1F8}")
