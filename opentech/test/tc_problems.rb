require 'battery_low'

include Opentech

class TestLogLine < Test::Unit::TestCase

  def setup
    @ll1 = LogDataLine.new("[11:06:47]  EB00048|ZA000040|00000551|?|")
    @ll2 = LogDataLine.new("[11:06:47]  EB00048|ZA000040|00000551|>|")
    @ll3 = LogDataLine.new("[11:06:47]  EB00048|ZA000040|00000551|2|")
  end

  def test_has_prob
    assert_equal(@ll1.count, 1)
    assert_equal(@ll2.count, 1)
    assert_equal(@ll3.count, 0)
  end

end

class TestProblems < Test::Unit::TestCase
  def setup
    arr = ["[11:06:47]  EB00048|ZA000040|00000551|?|",
           "[11:06:47]  EB00048|ZA000040|00000551|>|",
           "[11:06:47]  EB00048|ZA000040|00000551|2|"
    ]

    @ps = Problems.new(arr)
  end

  def test_ok
  end

end
