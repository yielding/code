#!/usr/bin/env ruby

require "minitest/autorun"

class FooTest < MiniTest::Test
  WIN32 = true

  def test_foo
    assert_equal('foo', 'foo')
  end

  def test_refutation
    refute_equal 'foo', 'bar'
  end

  def test_skip
    return skip if WIN32
    assert_equal('fun!', 'fun!')
  end
end
