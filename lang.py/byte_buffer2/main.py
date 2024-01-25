#!/usr/bin/env python

from br import *
from fatarea import *
from dentry import *
from node import *
from extent import *

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

    def expand(self, node):
        if node.is_file():
            raise Exception(f"file is given in expand_all")

        bb = ByteBuffer2(node.read_all())
        while bb.has_remaining(): 
            de = DirectoryEntry(bb)
            if de.lfn(): continue
            if de.empty(): break
            print(de)

    def expand_all(self, node):
        pass

    def build_leaf(self):
        file_addr = 0x404040
        self.file.seek(file_addr); 
        b0 = self.file.read(0x20)
        leaf = DirectoryEntry(b0)
        node = self.make_node(leaf)
        print(node)
        for e in node.extents:
            print(e)

    def make_root_node(self):
        stream = (self.file, self.read_root_exts())
        return Node('/', stream, NodeType.Dir)

    def read_root_exts(self):
        return self.read_extents(self.br.root_cluster_no)

    def read_extents(self, cluster_no):
        res = []
        bps = self.br.sector_size
        next = self.br.root_cluster_no
        while True:
            offset = (next - 2) * bps + self.br.data_area
            res.append(Extent(offset, self.br.cluster_size))
            next = self.fat0.fat[next]
            if next == 0xfffffff:
                break
            
        return res

    def make_node(self, dentry):
        type = NodeType.File if dentry.is_file else NodeType.Dir
        extents = self.to_extents(dentry.cluster_no)
        node = Node(dentry.name, (self.file, extents), type)
        node.size = dentry.size
        return node

    def to_extents(self, cluster_no):
        extents = []
        next = cluster_no
        while next != 0xfffffff:
            offset = self.br.data_area + (next-2)*self.br.cluster_size
            size   = self.br.cluster_size
            extents.append(Extent(offset, size))
            next = self.fat0.fat[next]

        return extents

if __name__ == "__main__":
    fat32 = Fat32("fat32.mdf")
    #fat32.build_filesystem()
    #fat32.build_leaf()
    root = fat32.make_root_node()
    fat32.expand(root)
    

    #all_files = fat32.search_all()
    #leaf = fat32.get("/DIRT/LEAF.jpg")
    #leaf.export_to("path")

    #leaf.export_to(file, "/Users/yielding/Desktop/leaf.jpg")
    #bb.offset(0x70)
    #print(hex(bb.get_uint4_be()))
