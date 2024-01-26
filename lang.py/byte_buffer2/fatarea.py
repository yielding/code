#!/usr/bin/env python

from byte_buffer2 import *

class FatArea:
    def __init__(self, buffer):
        self.entry_count = len(buffer) // 4
        bb = ByteBuffer2(buffer)
        self.fat = []  
        for i in range(0, self.entry_count):
            self.fat.append(bb.get_uint4_le())

    def all_clusters(self, start):
        clusters = []
        next = start
        while next != 0xfffffff:
            clusters.append(next)
            next = self.fat[next]

        return clusters

    def __str__(self):
        pass


if __name__ == "__main__":
    file = open("fat32.mdf", 'rb')
    file.seek(0x215c00)
    b0 = file.read(0x200)
    fat0 = FatArea(b0)
    print(br)
