#!/usr/bin/env python

import unittest
from byte_buffer2 import *

class TestByteBuffer2(unittest.TestCase):
  def setUp(self):
    print("setUp")
    self.testData = b'\x11\x22\x33\x44'
    self.bb = ByteBuffer2(self.testData)

  def tearDown(self):
    print("tearDown")

  def test_ctor(self):
    self.assertTrue(self.bb is not None)
    self.assertEqual(self.bb.size(), 4)

  def test_uint2_be(self):
    self.assertEqual(self.bb.get_uint2_be(), 0x1122)
    self.assertEqual(self.bb.offset(), 2)

  def test_uint2_le(self):
    self.assertEqual(self.bb.get_uint2_le(), 0x2211)
    self.assertEqual(self.bb.offset(), 2)

if __name__ == "__main__":
  unittest.main()
