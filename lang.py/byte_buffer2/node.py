#!/usr/bin/env python

from byte_buffer2 import *
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
        self.source, self.extents = stream 
        self.alloc_size = sum(map(lambda e: e.size, self.extents))
        self.size = self.alloc_size
        self.parent = None
        self.children = []
        self.type = type

    def is_file(self):
        return self.type == NodeType.File

    def is_dir(self):
        return self.type == NodeType.Dir

    def read_all(self):
        res = bytearray()
        file = self.source
        for extent in self.extents:
            file.seek(extent.offset)
            res.extend(file.read(extent.size))

        return bytes(res)

    def export_to(self, path):
        with open(path, 'wb') as file:
            for extent in self.extents:
                addr, size = extent
                source.seek(addr)
                b = source.read(size)
                file.write(b)

    def __str__(self) -> str:
        return f"name: {self.name}, size:{hex(self.size)}"
                        
if __name__ == "__main__":
    pass
