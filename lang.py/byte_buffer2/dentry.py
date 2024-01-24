#!/usr/bin/env python

from byte_buffer2 import *

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

        bb.offset = 0x14; cluster_hi = bb.get_uint2_le()
        bb.offset = 0x1a; cluster_lo = bb.get_uint2_le()
        self.cluster_no = (cluster_hi << 16) | cluster_lo

        bb.offset = 0x1c
        self.size = bb.get_uint4_le()

    def __str__(self) -> str:
        attr = "f" if self.is_file else "d"
        return f"attr: {attr}, name: {self.name}, cluster_no: {self.cluster_no}"

if __name__ == "__main__":
    pass
