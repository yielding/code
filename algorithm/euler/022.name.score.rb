#!/usr/bin/env ruby19

require "test/unit"

class String
  def score
    self.bytes.to_a.reduce(0) { |s, b| s + b - 64 }
  end
end

class TestStringScore < Test::Unit::TestCase
  def setup
    @arr = File.readlines("names.txt")[0].gsub(/"/, '').split(",").sort
  end

  def test_to_lower
    assert_equal("ABC".downcase!, "abc")
    assert_equal("a".ord, 97)
    assert_equal("A".ord, 65)
  end

  def test_apple
    assert_equal("A".score, 1) 
    assert_equal("APPLE".score, 1 + 12 + 16 + 16 + 5)
  end

  def test_dict
    assert_equal(871198282, 
                 @arr.map.with_index { |s, i| s.score * (i + 1) }.reduce(:+))
  end
end
