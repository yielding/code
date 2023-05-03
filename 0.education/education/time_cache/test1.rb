require 'pp'
require 'test/unit'

include DataModel

class TestCache < Test::Unit::TestCase

  def setup
    @cache = RFSignalCache.new
    # time, tid, rid, rssi, value, status
    @cache.cache(1, 1, 100, 0, 128)
    @cache.cache(1, 2, 100, 0, 128)
  end

  def test_cache_size
    assert_equal 2, @cache.count
  end

  def test_signal_within
    arr = @cache.get_signals_within(1).map { |signal| signal.m_tid }
    assert_equal arr, [1, 2]

    @cache.cache(1, 3, 100, 0, 128)
    arr = @cache.get_signals_within(1).map { |signal| signal.m_tid }
    assert_equal arr, [1, 2, 3]
  end

  def test_rid_index
    arr = @cache.get_signals_in_reader(1).map { |signal| signal.m_tid }
    assert_equal arr, [1, 2]

    @cache.cache(2, 3, 100, 0, 128)
    @cache.cache(2, 4, 100, 0, 128)
    @cache.cache(2, 5, 100, 0, 128)
    @cache.cache(3, 6, 100, 0, 128)
    @cache.cache(3, 7, 100, 0, 128)
    @cache.cache(3, 8, 100, 0, 128)
    arr = @cache.get_signals_in_reader(2).map { |signal| signal.m_tid }
    assert_equal arr, [3, 4, 5]

    @cache.get_signals_in_reader(3).each { |signal| arr << signal.m_tid }
    assert_equal arr, [3, 4, 5, 6, 7, 8]
  end
end

Test::Unit::AutoRunner.run
