#!/usr/bin/env swift

let greeting  = "Hello, world"
let index     = greeting.index(of: ",") ?? greeting.endIndex
let beginning = greeting[..<index]

let ns = String(beginning)
print(ns)

let dogString = "이창하"

for codeUnit in dogString.utf8 {
  let cu = String(codeUnit, radix: 16)
  print("0x\(cu) ", terminator: "")
}
