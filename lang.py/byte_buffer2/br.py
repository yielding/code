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

    def __str__(self) -> str:
        cs = hex(self.cluster_size)
        da = hex(self.data_area) 

        return f"cluster_size: {cs}, data_area: {da}"

if __name__ == "__main__":
    file = open("fat32.mdf", 'rb')
    b0 = file.read(0x200)
    br = BootRecord(b0)
    print(br)
