#!/usr/bin/env swift

import Foundation 

var ss = try String(contentsOfFile: "/Users/yielding/.vimrc", encoding: NSUTF8StringEncoding)

var arr = ss.componentsSeparatedByString("\r\n")

for s in arr { print(s) }
