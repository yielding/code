#!/usr/bin/env python

from byte_buffer2 import *

import pprint

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

class DirectoryEntry:
    def __init__(self, buffer, fat) -> None:
        bb = ByteBuffer2(buffer)

        name = bb.get_ascii(8).rstrip()
        ext  = bb.get_ascii(3).rstrip()
        self.name = f'{name}.{ext}'

        bb.offset = 0x0b
        attr = bb.get_uint1()
        self.is_file = (attr & 0x20) == 0x20
        self.is_dir  = (attr & 0x10) == 0x10

        bb.offset  = 0x14; cluster_hi = bb.get_uint2_le()
        bb.offset  = 0x1a; cluster_lo = bb.get_uint2_le()
        self.cluster_no = (cluster_hi << 16) | cluster_lo

        bb.offset  = 0x1c
        self.size  = bb.get_uint4_le()

    def __str__(self) -> str:
        attr = "f" if self.is_file else "d"
        return f"attr: {attr}, name: {self.name}, cluster_no: {self.cluster_no}"

class Node:
    def __init__(self) -> None:
        self.name = ""
        self.size = -1
        self.source = file
        self.extents = [] # (404000, 1000), (405000, 1000)

    def export_to(self, path):
        with open(path, 'wb') as file:
            for extent in self.extents:
                addr, size = extent
                source.seek(addr)
                b = source.read(size)
                file.write(b)

class Fat32:
    def __init__(self, path) -> None:
        self.br = self.fat0 = None
        self.file = open(path, 'rb')

        b0 = self.file.read(0x200)
        self.br = BootRecord(b0)
        self.file.seek(self.br.fat_area_addr); 

        b0 = self.file.read(self.br.fat_area_size)
        self.fat0 = FatArea(b0)

    def build(self):
        file_addr = 0x404040
        self.file.seek(file_addr); 
        b0 = self.file.read(0x20)
        leaf = DirectoryEntry(b0, self.fat0)
        print(leaf)

    def to_node(self, dentry):
        node = Node()
        node.size = dentry.size
        node.extents = to_extents(dentry.cluster_no)
        return node

    def to_extents(cluster_no):
        extents = []
        next = cluster_no
        while next != 0xfffffff:
            #offset = br.data_area + (next-2)*br.cluster_size
            offset = 0x400000 + (next-2)*self.br.cluster_size
            size = self.br.cluster_size
            extents.append((offset, size))
            next = self.fat0.fat[next]

        return extents

if __name__ == "__main__":
    fat32 = Fat32("fat32.mdf")
    fat32.build()

    #all_files = fat32.search_all()
    #leaf = fat32.get("/DIRT/LEAF.jpg")
    #leaf.export_to("path")

    #leaf.export_to(file, "/Users/yielding/Desktop/leaf.jpg")
    #bb.offset(0x70)
    #print(hex(bb.get_uint4_be()))
