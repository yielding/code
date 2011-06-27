require "test/unit"

require "skype/chat_log"

class TestSkype < Test::Unit::TestCase
  def setup
    call256 = File.expand_path(File.dirname(__FILE__)) + "/../data/call256.dbb"
    @chat = Skype::ChatLog.new(call256)
    @chat.open
  end

  def test_open
    assert @chat.open?
  end

  def test_get_record
    #assert_equal(@skype.offset, 0)
    #r = @skype.get_record
    #assert_equal(r[:length], 0x47)
    #assert_equal(r[:sequence], 0x15)
  end
end
