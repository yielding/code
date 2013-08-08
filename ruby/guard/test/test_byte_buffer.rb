require "test/unit"
require "pp"

require "byte_buffer"

class TestByteBuffer < Test::Unit::TestCase
  def setup
    @b = ByteBuffer.new
  end

  def test_int2
    @b.set_int2_be(1000) # 10000 = 0x3E8
    @b.flip
    assert_equal(@b.get_int2_be, 1000)

    @b.flip
    assert_equal(@b.get_int1, 0x03)
    assert_equal(@b.get_int1, 0xE8)

    @b.reset
    @b.set_int2_le(1000) # 10000 = 0x3E8
    @b.flip
    assert_equal(@b.get_int2_le, 1000)

    @b.flip
    assert_equal(@b.get_int1, 0xE8)
    assert_equal(@b.get_int1, 0x03)
  end

  def test_int4
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
  end

  def test_int8

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
  end

  def test_get_varint
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
  end
end
