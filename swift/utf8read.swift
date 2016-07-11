#!/usr/bin/env swift

import Foundation 

var ss = try String(contentsOfFile: "/Users/yielding/.vimrc", encoding: String.Encoding.utf8)

var arr = ss.components(separatedBy: "\r\n")
for s in arr { print(s) }
