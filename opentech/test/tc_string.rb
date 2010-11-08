class TestString < Test::Unit::TestCase
  def setup
    @s1 = "[07:06:47]  EB00048|ZA000040|00000551|3|"
    @s2 = "[11:06:47]  EB00048|ZA000040|00000551|3|"
    @s3 = "[07:06:46]  EB00202|ZC000001|V3.3|\n"
    @s4 = "[11:06:47]  EB00048|ZA000040|00000551|?|"
  end
  
  def test_afternoon_morning
    assert_equal(@s1.afternoon?, false)
    assert_equal(@s1.morning?, true)
  end
  
  def test_wrong
    assert_equal(@s2.afternoon?, false)
    assert_equal(@s2.morning?, false)
  end
  
  def test_data_and_link
    assert_equal(@s1.data?, true)
    assert_equal(@s3.data?, false)
    assert_equal(@s3.link_message?, true)
  end
  
  def test_has_prob
    assert_equal(@s1.has_problem?, false)
    assert_equal(@s4.has_problem?, true)  
  end
end
