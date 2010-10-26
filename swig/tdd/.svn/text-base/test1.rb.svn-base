require 'test/unit'

#require 'DataModel' # for bundle

include DataModel

class TestSum < Test::Unit::TestCase
  def setup
    @data = Ints.new([])
    1.upto(10) do |i| @data.push(i) end
  end

  def test_sum
    assert_equal 55, sum(@data)
  end
  
  def test_mean
    assert_equal 5.5, mean(@data)
  end

  def test_even
    assert_equal [2, 4, 6, 8, 10], @data.find_all { |x| x % 2 == 0 }
  end

  def test_minmax
    assert_equal 1 , @data.min
    assert_equal 10, @data.max
  end
  
end

Test::Unit::AutoRunner.run
