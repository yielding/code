#!/usr/bin/env ruby

require 'DataModel'
require 'test/unit'

include DataModel

class TestSum < Test::Unit::TestCase
  def setup
    @data = Ints.new
    1.upto(10) do |i| @data.push(i) end
  end

  def test_sum
    assert_equal 55, sum(@data)
  end
  
  def test_mean
    assert_equal 5.5, mean(@data)
  end
end

Test::Unit::AutoRunner.run
