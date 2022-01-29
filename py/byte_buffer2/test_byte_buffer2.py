#!/usr/bin/env python

import unittest
from byte_buffer2 import *

class TestByteBuffer2(unittest.TestCase):
    def setUp(self):
        self.t1 = b'\x11\x22\x33\x44'
        self.d0 = b'hello\x00world\x00'
        self.d1 = b'\x20\x00I\x00n\x00f\x00'
        self.bb = ByteBuffer2(self.t1)

    def test_ctor(self):
        self.assertTrue(self.bb is not None)
        self.assertEqual(self.bb.size(), 4)

    def test_uint2_be(self):
        self.assertEqual(self.bb.get_uint2_be(), 0x1122)
        self.assertEqual(self.bb.offset(), 2)

    def test_uint2_le(self):
        self.assertEqual(self.bb.get_uint2_le(), 0x2211)
        self.assertEqual(self.bb.offset(), 2)

    def test_ascii(self):
        bb = ByteBuffer2(self.d0)
        r0 = bb.get_ascii()
        self.assertEqual(r0, 'hello')
        self.assertEqual(bb.offset(), 6)

        r1 = bb.get_ascii()
        self.assertEqual(r1, 'world')
        self.assertEqual(bb.offset(), 12)

    def test_utf16le(self):
        bb = ByteBuffer2(self.d1)
        r0 = bb.get_utf16_le(4)
        self.assertEqual(r0, ' Inf')

if __name__ == "__main__":
    unittest.main()
