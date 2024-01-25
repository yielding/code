#!/usr/bin/env python

from byte_buffer2 import *

class DirectoryEntry:
    def __init__(self, buffer) -> None:
        bb = buffer if isinstance(buffer, ByteBuffer2) else ByteBuffer2(buffer)

        self.is_lfn = False
        self.is_empty = bb.compare_range(bb.offset, 0x20, 0)
        if self.is_empty: return

        backup = bb.offset

        try:
            self.is_deleted = bb.get_uint1() == 0xe5
            bb.unget(1)
            if self.is_deleted: bb.change_cur_to('?')

            name = bb.get_ascii(8).rstrip()
            ext  = bb.get_ascii(3).rstrip()

            self.name = f'{name}.{ext}' if len(ext) > 0 else name

            self.attr = bb.get_uint1()
            self.is_vol  = (self.attr & 0x08) == 0x08
            self.is_file = (self.attr & 0x20) == 0x20
            self.is_dir  = (self.attr & 0x10) == 0x10
            self.is_lfn  = (self.attr & 0x0f) == 0x0f

            bb.skip(8)
            cluster_hi = bb.get_uint2_le()
            bb.skip(4)
            cluster_lo = bb.get_uint2_le()
            self.cluster_no = (cluster_hi << 16) | cluster_lo
            self.size = bb.get_uint4_le()
        finally:
            bb.offset = backup + 0x20

    def empty(self):
        return self.is_empty

    def lfn(self):
        return self.is_lfn

    def vol(self):
        return self.is_vol

    def deleted(self):
        return self.is_deleted

    def __get_attr(self, attr):
        if (attr & 0x08) == 0x08: return 'v'
        if (attr & 0x10) == 0x10: return 'd'
        if (attr & 0x20) == 0x20: return 'f'

    def __str__(self) -> str:
        a = self.__get_attr(self.attr)
        c = hex(self.cluster_no)
        s = hex(self.size)
        n = self.name
        return f"attr: {a}, name: {n}, cluster_no: {c}, size: {s}"

if __name__ == "__main__":
    file = open("fat32.mdf", 'rb')
    file.seek(0x404040)
    b0 = file.read(0x20)
    bb = ByteBuffer2(b0)
    br = DirectoryEntry(bb)
    print(br)
