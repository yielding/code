#!/usr/bin/env python

from byte_buffer2 import *

class BootRecord:
    def __init__(self, buffer):
        bb = ByteBuffer2(buffer)
        bb.offset = 0x0b
        self.sector_size  = bb.get_uint2_le()
        self.sector_count = bb.get_uint1()
        self.cluster_size = self.sector_count * self.sector_size

        bb.offset = 0x30-4
        self.root_cluster_no = bb.get_uint4_le()

        bb.offset = 0x0e
        self.fat_area_addr = bb.get_uint2_le() * self.sector_size

        bb.offset = 0x24
        self.fat_area_size = bb.get_uint4_le() * self.sector_size

        bb.offset = 0x10
        no_of_fat = bb.get_uint1()
        self.data_area = self.fat_area_addr + self.fat_area_size * no_of_fat

if __name__ == "__main__":
    pass

    #all_files = fat32.search_all()
    #leaf = fat32.get("/DIRT/LEAF.jpg")
    #leaf.export_to("path")

    #leaf.export_to(file, "/Users/yielding/Desktop/leaf.jpg")
    #bb.offset(0x70)
    #print(hex(bb.get_uint4_be()))
