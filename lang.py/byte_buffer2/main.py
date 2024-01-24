#!/usr/bin/env python

from byte_buffer2 import *
from br import *
from fatarea import *
from dentry import *
from enum import Enum

class NodeType(Enum):
    File = 1
    Dir = 2
    SymLink = 3
    HardLink = 4

class Node:
    def __init__(self, name, stream, type=NodeType.File) -> None:
        self.name = name
        self.path = ""
        self.size = -1
        # stream
        self.source, self.extents = stream 
        self.parent = None
        self.children = []
        self.type = type

    def export_to(self, path):
        with open(path, 'wb') as file:
            for extent in self.extents:
                addr, size = extent
                source.seek(addr)
                b = source.read(size)
                file.write(b)

    def __str__(self) -> str:
        return f"name: {self.name}"
                        
class Fat32:
    def __init__(self, path) -> None:
        self.br = self.fat0 = None
        self.file = open(path, 'rb')

        b0 = self.file.read(0x200)
        self.br = BootRecord(b0)
        self.file.seek(self.br.fat_area_addr); 

        b0 = self.file.read(self.br.fat_area_size)
        self.fat0 = FatArea(b0)

    def build_filesystem(self):
        self.root = self.make_root_node()
        print(self.root)

    def build_leaf(self):
        file_addr = 0x404040
        self.file.seek(file_addr); 
        b0 = self.file.read(0x20)
        leaf = DirectoryEntry(b0, self.fat0)
        print(leaf)

    def make_node(self):
        pass

    def make_root_node(self):
        stream = (self.file, self.read_root_exts())
        return Node('/', stream, NodeType.Dir)

    def read_root_exts(self):
        return self.read_extents(self.br.root_cluster_no)

    # extent = (start_offset, size)
    def read_extents(self, cluster_no):
        res = []
        bps = self.br.sector_size
        next = self.br.root_cluster_no
        while True:
            offset = (next - 2) * bps + self.br.data_area
            res.append((offset, self.br.cluster_size))
            next = self.fat0.fat[next]
            if next == 0xfffffff:
                break
            
        return res

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
    fat32.build_filesystem()
    

    #all_files = fat32.search_all()
    #leaf = fat32.get("/DIRT/LEAF.jpg")
    #leaf.export_to("path")

    #leaf.export_to(file, "/Users/yielding/Desktop/leaf.jpg")
    #bb.offset(0x70)
    #print(hex(bb.get_uint4_be()))
