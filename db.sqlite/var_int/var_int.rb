#!/usr/bin/env ruby

require "test/unit"

def parse_varint(bytes, offset)
  viBytes = bytes[offset..-1]
  byteNo  = 0
  value   = 0
  complete = false

  while (byteNo < 9 and byteNo < viBytes.size and not complete)
    viByte = viBytes[byteNo]
    if ((viByte & 0b10000000) == 0b10000000 and byteNo < 8)
      value = (value << 7) | (viByte & 0b01111111)
    elsif ((viByte & 0b10000000) == 0b10000000 and byteNo == 8)
      value = (value << 8) | (viByte)
      complete = true
    else
      value = (value << 7) | (viByte & 0b01111111)
      complete = true
    end
    byteNo += 1
  end

  raise "No valid varint found" unless complete
  return byteNo, value
end

class TestVarInt < Test::Unit::TestCase
  def test_true
    arr = [1, 2, 3, 4, 5]
    assert_equal(arr[2..-1], [3, 4, 5])
  end

  def test_get_varint2
    buffer = [0x81, 0x7f]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xff)
  end

  def test_get_varint3
    buffer = [0x83, 0xff, 0x7f]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xffff)
  end

  def test_get_varint4
    buffer = [0x87, 0xff, 0xff, 0x7f]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xffffff)
  end

  def test_get_varint5
    buffer = [0x8f, 0xff, 0xff, 0xff, 0x7f]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xffffffff)
  end

  def test_get_varint6
    buffer = [0x9f, 0xff, 0xff, 0xff, 0xff, 0x7f]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xffffffffff)
  end

  def test_get_varint7
    buffer = [0xbf, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xffffffffffff)
  end

  def test_get_varint8
    buffer = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xffffffffffffff)
  end

  def test_get_varint9
    buffer = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
    byte_no, value = parse_varint(buffer, 0)
    assert_equal(value, 0xffffffffffffffff)
  end
end
