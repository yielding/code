#!/usr/bin/env ruby

require "test/unit"

# S = { a, b, c, d, e }
#     { T, T, T, T, T }
#     { T, T, T, T, F }
#     { T, T, T, F, T }
#     { T, T, T, F, F }

class Backtrack
  def initialize
    @org = nil
  end

  def identity; @org end

  def subset arr; @org = arr end

  def solution? curr
    @org.length == curr  
  end

  def process_solution
  end

  def construct_candidate
    candidate = [true, false]
  end

  def backtrack(a, depth)
    if solution? depth 
      process_solution
    else
      depth += 1
      construct_candidate.each {|c|
        a[depth] = c
        backtrack(a, depth)
      }
    end
  end
end

#class TestBacktrack < Test::Unit::TestCase
#  def setup
#    @bt = Backtrack.new
#  end
#
#  def test_identity
#    @bt.subset([1])
#    assert_equal @bt.identity, [1]
#
#    @bt.subset([1, 2])
#    assert_equal @bt.identity, [1, 2]
#  end
#end

if __FILE__ == $PROGRAM_NAME
  
end
