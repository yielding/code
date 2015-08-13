#!/usr/bin/evn swift

extension String {
    var length: Int { return characters.count }
}

extension Int {
    var toHex: Int { return  }
}

var arr = [9, 1, 3, 5, 7]
print(arr.sort(<))

var reserved = arr.sort( {(s1, s2) in return s1 > s2 })
print(reserved)

var arr2:[String] = ["Karen", "Annie~~~", "Mary"]

var xx = arr2.sort() { $0.length < $1.length }
print(xx)

let digitNames = [
    0:"Zero", 1:"One", 2:"Two", 3:"Three",
    5:"Five", 6:"Six", 7:"Seven", 8:"Eight", 9:"Nine"
]

let strings = arr.map {
    (var no) -> String in
    var output = ""
    while no > 0 {
        output = digitNames[no % 10]! + output
        no /= 10
    }

    return output
}

print(0xc0000 * 0x1000)
