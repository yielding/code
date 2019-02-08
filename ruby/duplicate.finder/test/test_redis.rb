require "helper"

class TestRedisIO < MiniTest::Test
  def setup
    @hs = HashSet.new
    @hs.clear
  end

  def test_basic
    assert(@hs.add(1))
    assert_equal(1, @hs.count_in_cache)
    res = @hs.add(1)
    assert_equal(res, false)
    assert_equal(1, @hs.count_in_cache)
    assert(@hs.add(2))
    assert_equal(2, @hs.count_in_cache)
  end

  def test_hashes
    10.downto(1) { |i| @hs.add(i) }
    assert_equal(@hs.size, 10)
    expected = [*1..10].map { |e| e.to_s }
    assert_equal(expected, @hs.members)
  end
end

