require "test/unit"

require "skype/call_history"

class TestChatHistory < Test::Unit::TestCase
  def setup
    call256 = File.expand_path(File.dirname(__FILE__)) + "/../data/call256.dbb"
    @call = Skype::CallHistory.new(call256)
    @call.open
  end

  def test_open
    assert @call.open?
  end

  def test_get_record
    @call.parse
    # size, id, ts, name, key, dir, duration = @call.get_record
    # assert_equal(size, 0x42)
    # assert_equal(id, 0x17)
    # assert_equal(ts, 1309310160)
    # assert_equal(name, "gmd_system")
    # assert_equal(key, "1-1309310160")
    # assert_equal(dir, 0)
    # assert_equal(duration, 0)
  end
end
