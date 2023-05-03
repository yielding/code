#!/usr/bin/env swift

func swap<T>(inout a: T, inout b: T) {
  let temp = a
  a = b
  b = temp
}

var a = 10
var b = 20

swap(&a, &b)
print(a)

var aa = "lee"
var bb = "min"

swap(&aa, &bb)
print(aa)
