#!/usr/bin/env swift

var possibleNo: String? = "hi"

if let actualNumber = Int(possibleNo!) {
    print("ok")
} else {
    print("not ok")
}

if let first = Int("5"), second = Int("42") 
    where first < second {
    print("ok2")
}


let optinalString: String? = "An optional string."

// ! is a must
let foredString: String = optinalString!


let assumedString : String! = "An implicitly Unwrapped"

// no need for an !
let implicitString: String  = assumedString
