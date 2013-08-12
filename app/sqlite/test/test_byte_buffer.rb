#!/usr/bin/env ruby

require "test/unit"
require "pp"

require_relative "byte_buffer"

class TestByteBuffer < Test::Unit::TestCase
  def setup
    @bytes = [1, 2, 3, 4, 5, 6, 7, 8]
    @b     = ByteBuffer.new(@bytes)
  end

  def test_int1
    @bytes.each { |byte| assert_equal(byte, @b.get_int1) }
    assert_raise(IndexError) { @b.get_int1 }

    @b.pos = 7
    assert_equal(@b.get_int1, 8)
  end

  def test_int2_be
    actual = @b.get_int2_be
    assert_equal((1<<8) + 2, actual)
    assert_equal(258, actual)

    @b.pos = 6
    assert_equal((7<<8) + 8, @b.get_int2_be)

    @b.pos = 7
    assert_raise(IndexError) { @b.get_int2_be }
  end

  def test_int2_le
    actual = @b.get_int2_le
    assert_equal((2<<8) + 1, actual)
    assert_equal(513, actual)

    @b.pos = 7
    assert_raise(IndexError) { @b.get_int2_le }
  end

  def test_uint2_be
    actual = @b.get_int2_be
    assert_equal((1<<8) + 2, actual)
    assert_equal(258, actual)

    @b.pos = 6
    assert_equal((7<<8) + 8, @b.get_int2_be)

    @b.pos = 7
    assert_raise(IndexError) { @b.get_int2_be }
  end

  def test_uint2_le
    actual = @b.get_int2_le
    assert_equal((2<<8) + 1, actual)
    assert_equal(513, actual)

    @b.pos = 7
    assert_raise(IndexError) { @b.get_int2_le }
  end

=begin rdoc

    @b.reset
    @b.set_int2_le(0x1234) # 10000 = 0x3E8
    @b.flip
    assert_equal(@b.get_int2_le, 0x3412)

    @b.flip
    assert_equal(@b.get_int1, 0x34)
    assert_equal(@b.get_int1, 0x12)
=end

  def test_int4
=begin rdoc
    
    @b.set_int4_be(0x12345678)
    @b.flip
    assert_equal(@b.get_int4_be, 0x12345678)

    @b.flip
    arr = [0x12, 0x34, 0x56, 0x78]
    arr.each { |no| assert_equal(@b.get_int1, no) }

    @b.reset
    @b.set_int4_le(0x12345678)
    @b.flip
    assert_equal(@b.get_int4_le, 0x12345678)

    @b.flip
    arr.reverse.each { |no| assert_equal(@b.get_int1, no) }
=end
  end

  def test_int8
=begin rdoc
    
    @b.set_int8_be(0x1122334455667788)
    @b.flip
    assert_equal(@b.get_int8_be, 0x1122334455667788)

    @b.flip
    arr = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
    arr.each { |no| assert_equal(@b.get_int1, no) }

    @b.reset
    @b.set_int8_le(0x1122334455667788)
    @b.flip
    assert_equal(@b.get_int8_le, 0x1122334455667788)

    @b.flip
    arr.reverse.each { |no| assert_equal(@b.get_int1, no) }
=end
  end

  def test_get_varint
=begin rdoc
        
    @b.from_array [0x81, 0x7f]
    assert_equal(@b.get_varint, 0xff)

    @b.from_array [0x83, 0xff, 0x7f]
    assert_equal(@b.get_varint, 0xffff)

    @b.from_array [0x87, 0xff, 0xff, 0x7f]
    assert_equal(@b.get_varint, 0xffffff)

    @b.from_array [0x8f, 0xff, 0xff, 0xff, 0x7f]
    assert_equal(@b.get_varint, 0xffffffff)

    @b.from_array [0x9f, 0xff, 0xff, 0xff, 0xff, 0x7f]
    assert_equal(@b.get_varint, 0xffffffffff)

    @b.from_array [0xbf, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]
    assert_equal(@b.get_varint, 0xffffffffffff)

    @b.from_array [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]
    assert_equal(@b.get_varint, 0xffffffffffffff)

    @b.from_array [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
    assert_equal(@b.get_varint, 0xffffffffffffffff)
=end
  end
end
