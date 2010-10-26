require "test/unit"

require "fibo"

class TestFibo < Test::Unit::TestCase
  def setup
    @fibo = Fibo.new
  end
  
  def test_1_should_be_1
    assert_equal(1, Fibo.fibo(1))
  end

  def test_2_should_be_1
    assert_equal(1, Fibo.fibo(2))
  end
  
  def test_3_should_be_2                                 
    assert_equal(2, Fibo.fibo(3))
  end
end                            
