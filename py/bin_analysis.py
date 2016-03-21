#!/usr/bin/env python
# encoding: utf-8

import struct

def hex_str_to_binary(s):
    ints = [int(s[i:i+2], 16) for i in range(0, len(s), 2)]
    return "".join(map(lambda x:struct.pack("B", x), ints))

s = hex_str_to_binary("636F6D2E676F6F67")
p = hex_str_to_binary("401225")

s = open("/Users/yielding/Desktop/174.bin", "r").read()
print ord(s[0x3d7])

