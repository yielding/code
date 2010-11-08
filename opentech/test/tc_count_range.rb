require "count_range"

class TestCountRange < Test::Unit::TestCase
  def setup
    lines = [
      "[07:06:47]  EB00048|ZA000040|00000001|3|",
      "[11:06:47]  EB00048|ZA000040|00000002|3|",
      "[07:06:46]  EB00202|ZC000001|V3.3|\n",
      "[11:06:47]  EB00048|ZA000040|00000003|?|",
      "[11:06:47]  EB00048|ZA000040|00000004|?|",
      "[11:06:47]  EB00048|ZA000040|00000005|?|",
      "[11:06:47]  EB00048|ZA000040|00000006|?|",
      "[11:06:47]  EB00048|ZA000040|00000007|?|",
      "[11:06:47]  EB00048|ZA000040|00000008|?|",
      "[11:06:47]  EB00048|ZA000040|00000009|?|",
      "[11:06:47]  EB00048|ZA000040|00000010|?|"
    ]
    @cr = CountRange.new(lines, 1)
  end
  
  def test_stat
    assert_equal(@cr.max, 1)
    assert_equal(@cr.min, 1)
    assert_equal(@cr.average, 1.0)
  end
  
end

