#!/usr/bin/env swift

import Foundation 

var ss = try String(contentsOfFile: "/Users/yielding/.vimrc", encoding: NSUTF8StringEncoding)

var arr = ss.components(separatedBy: "\r\n")
for s in arr { print(s) }
