require 'test/unit'

# 나중에는 Array클래이스안에 subsum method를 넣고
# main은 [31, -41, 59, 26, -53, 58, 97, -93, -23, 84].subsum()
# 이렇게

class ArrayProb
  def initialize
    @arr = [31, -41, 59, 26, -53, 58, 97, -93, -23, 84]
  end

  def maxsum1(from=0, to=@arr.length-1)
    maxsofar = 0
    from.upto(to) do |i|
      i.upto(to) { |j| maxsofar = max(maxsofar, partial_sum(i, j)) }
    end
    maxsofar
  end

  def maxsum2(from=0, to=@arr.length-1)
    return 0 if from > to
    return max(0, @arr[from]) if from == to

    mid = (from + to) / 2

    lmax = sum = 0
    mid.downto(from) { |i| sum += @arr[i]; lmax = max(lmax, sum) } 

    rmax = sum = 0
    (mid+1).upto(to) { |i| sum += @arr[i]; rmax = max(rmax, sum) }

    mid_sum = lmax + rmax
    return max3(mid_sum, maxsum2(from, mid), maxsum2(mid+1, to))
  end

  def maxsum3(from=0, to=@arr.length-1)
    maxsofar = 0
    maxendinhere = 0
    from.upto(to) do |i|
      maxendinhere = max(maxendinhere + @arr[i], 0)
      maxsofar = max(maxsofar, maxendinhere)
    end
    maxsofar
  end

  # private
  def partial_sum(from, to)
    (from..to).inject(0) { |sum, n| sum + @arr[n] }
  end

  def max(a, b)
    a >= b ? a : b
  end
  
  def max3(a, b, c)
    if a > b
      a > c ? a : c
    else
      b > c ? b : c
    end
  end
end

class TestArrayProb < Test::Unit::TestCase
  def setup
    @ap = ArrayProb.new
  end

  def test_max
    assert_equal @ap.max(1, 2), 2
    assert_equal @ap.max(2, 1), 2
  end

  def test_partial_sum
    assert_equal @ap.partial_sum(0, 9), 145
    assert_equal @ap.partial_sum(1, 2),  18
  end

  def test_max3
    assert_equal @ap.max3(1, 2, 3), 3
    assert_equal @ap.max3(1, 3, 2), 3
    assert_equal @ap.max3(2, 1, 3), 3
    assert_equal @ap.max3(2, 3, 1), 3
    assert_equal @ap.max3(3, 1, 2), 3
    assert_equal @ap.max3(3, 2, 1), 3
  end

  def test_middle
    @res1 = [0, 1, 1, 2, 2, 3, 3, 4, 4, 5]
    @res2 = (1..10).map { |i| i/2 }
    assert_equal @res1, @res2
  end

  def test_maxsum
    assert_equal @ap.maxsum1, 187
    assert_equal @ap.maxsum2, 187
    assert_equal @ap.maxsum3, 187
  end
end
