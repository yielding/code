require 'test/unit'

class TestSum < Test::Unit::TestCase
  def setup
  end

  def test_major
    assert_equal VIM::VIM_MAJOR, 7
  end
  def test_minor
    assert_equal VIM::VIM_MINOR, 2
  end
  
  def test_true
    assert_equal 1, 1
  end
end

#Test::Unit::AutoRunner.run
