#!/usr/bin/env swift

import Foundation

var bytes: [UInt8] = [ 0xff, 0xd8, 0xff ]
var data = NSData(bytes: bytes, length: bytes.count)

var header: UInt16 = 0
data.getBytes(&header, range: NSRange(location: 0, length: 2))

print(header.description)
header = UInt16(littleEndian: header)
print(header.description)

var a = UInt16(littleEndian: 0x1000)

print(a)
