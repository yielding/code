#!/usr/bin/env swift

import Foundation

extension UInt8 {
  func char() -> Character {
    return Character(UnicodeScalar(Int(self)))
  }
}

// 1. posix interface
// let fd = open("/Users/yielding/code/swift/arc.closure.swift", O_RDONLY)
// var buffer = [UInt8](repeating: 0, count:10)
// read(fd, &buffer, 10)
// for i in buffer { print(i.char()) }
// close(fd)

// 2. NSDAta 
// let data = NSData (contentsOfFile: "/Users/yielding/code/swift/arc.closure.swift")
// var rnge = NSRange(location: 0, length: 10)
// var buffer = [Int8](repeating: 0, count: 10)
// data!.getBytes(&buffer, range: rnge)
// print(data)

// JPEG endmark
// var endMarker = NSData(bytes: [0xFF, 0xD9] as [UInt8], length: 2)
