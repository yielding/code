require "test/unit"

require "structs"

class Internal < BinData::Record
  attr_reader :child

  endian :big

  uint16 :a
  uint16 :b

  def add_child c
    @child = c
  end

end

class TestStructs < Test::Unit::TestCase
  def setup
  end

  def teardown
  end

  def test_array
    arr = BinData::Array.new(:type => :uint8be, :initial_length => 10)
    offsets = arr.read("\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a")
    assert_equal(offsets, [*0x11..0x1a])
    assert_equal(arr, [*0x11..0x1a])
  end

  def test_additional

    index = Internal.new
    rec = index.read("\x00\x01\x02\x03")
    pp rec

    rec.add_child 10

    pp rec.child

  end
end
