#!/usr/bin/env python

from byte_buffer2 import *

class Superblock:
    def __init__(self, bb):
        bb.offset(0x30-4)
        self.root_inode = bb.get_uint4_le()

if __name__ == "__main__":
    buffer = None
    with open('fat32.mdf', 'rb') as file:
        buffer = file.read(0x200)

    bb = ByteBuffer2(buffer)
    bb.offset(0x20)
    print(hex(bb.get_uint4_be()))

    #sb = Superblock(bb)
    #print(hex(sb.root_inode))

    #bb.offset(0x70)
    #print(hex(bb.get_uint4_be()))
    