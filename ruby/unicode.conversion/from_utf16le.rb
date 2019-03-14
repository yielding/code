#!/usr/bin/env ruby
#encoding: utf-8

# part of stirng "System.." with utf16-le repr
arr = [0x53, 0x00, 0x79, 0x00, 0x73, 0x00] 

s0 = arr.pack("c*")
p s0.encoding #<Encoding:ASCII-8BIT>

s1 = s0.force_encoding("utf-16le")
p s1.encoding #<Encoding:UTF-16LE>
#s1.start_with?("S") # fail

s2 = s1.encode("utf-8")
p s2.encoding #<Encoding:UTF-8>
#s2.start_with?("S") # ok

s0 = "dog"
p s0.encoding #<Encoding:UTF-8>
s1 = s0.bytes.map(&:chr).join
p s1.encoding # dog: <Encoding:US-ASCII>

s2 = s1.encode("utf-8")
p s2.encoding # dog: <Encoding:UTF-8>
