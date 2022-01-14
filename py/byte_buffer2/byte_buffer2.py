#!/usr/bin/env python

from struct import *

class ByteBuffer2:
  def __init__(self, bs):
    self.m_data = bs
    self.m_offset = 0
    self.m_limit = len(bs)

  def size(self):
    return len(self.m_data)

  def offset(self):
    return self.m_offset

  def get_uint2_be(self):
    s = self.m_offset
    r = self.m_data[s:s+2]
    self.m_offset += 2

    return unpack('>H', r)[0]

  def get_uint2_le(self):
    s = self.m_offset
    r = self.m_data[s:s+2]
    self.m_offset += 2

    return unpack('<H', r)[0]
