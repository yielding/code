#!/usr/bin/env swift

import Foundation

let BAILOUT = 16
let MAX_ITERATIONS = 1000

func loop() -> Void {
    print("Rendering")
    for y in -39..<39 {
        print("")
        for x in -39..<39 {
            let i = mandel(Double(x)/40.0, y: Double(y)/40.0)
            if i == 0  { 
                print("*", appendNewline: false)
            } else {
                print(" ", appendNewline: false)
            }
        }
    }
}

func mandel(x: Double, y: Double) -> Int {
	let cr = y - 0.5
	let ci = x
  var zi = 0.0
  var zr = 0.0

  var i = 0
  while true {
    i += 1
    let temp = zr * zi
    let zr2  = zr * zr
    let zi2  = zi * zi
    zr = zr2 - zi2 + cr
    zi = temp + temp + ci
    if Int(zi2 + zr2) > BAILOUT { return i } 
    if i > MAX_ITERATIONS  { return 0 }
  }
}

var start = NSDate()
loop()
var end = NSDate()
var timeTaken = end.timeIntervalSinceDate(start)
print("")
print("Time taken: \(timeTaken) ms.")
