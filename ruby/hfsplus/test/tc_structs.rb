require "test/unit"

require "structs"

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
  end
end
