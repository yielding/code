#!/usr/bin/env ruby -wKU

#
# This function will be used in the ByteBuffer
# 
def parseVarInt(buffer, offset)
  byte_count = 0
  value      = 0
  complete   = false

  has_next = lambda { |value| (value & 0b10000000) == 0b10000000 }

  while (byte_count < 9 and not complete)
    val = buffer[offset]
    if (has_next(val) and byte_count < 8)
    elsif (has_next(val) and byte_count == 8)
      value = (value << 8) | val
      complete = true
    else
      value = (value << 7) | (val & 0b01111111)
      complete = true
    end

    byte_count += 1
  end

  raise "Invalid VarInt" unless complete

  return byte_count, value
end
