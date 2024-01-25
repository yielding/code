#!/usr/bin/env python

from byte_buffer2 import *

class Extent:
    def __init__(self, offset, size):
        self.offset = offset
        self.size  = size

    def __str__(self):
        return f"[{hex(self.offset)}, {hex(self.size)}]"

if __name__ == "__main__":
    print(Extent(4096, 800))
