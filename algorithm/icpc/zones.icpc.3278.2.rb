#!/usr/bin/env ruby

require 'test/unit'
require 'set'
require 'pp'

module Algorithm
  module Backtrack
    protected
    def backtrack(a, k, n)
      if solution?(a, k, n) then
        process_solution a, k
      else
        k += 1
        gen_candidates(a, k, n).each do |candidate|
          a[k] = candidate
          backtrack(a, k, n)
        end
      end
    end

    def solution? a, k, n
      k == n
    end

    def gen_candidates a, k, n
      c = []
    end

    def process_solution a, k
      print "{"
      for i in 1..k
        print a[i] ? " T " : " F "
      end
      print "}\n"
    end
  end
end

class Combination
  include Algorithm::Backtrack
  attr_reader :candidates

  def initialize n, r
    @n = n
    @r = r
    @candidates = []

    backtrack([0] * @n, 0, @n)
  end

  protected
  def gen_candidates a, k, n
    in_permutation = [false] * n
    k.times { |i| in_permutation[a[i]] = true }
    
    (1..n).select { |i| i unless in_permutation[i] }
  end

  def solution? a, k, n
    k == @r
  end

  def process_solution a, k
    c = a[1..k].sort
    @candidates << c unless @candidates.include? c
  end
end

class TestICPC < Test::Unit::TestCase
  def test_combination
    c1 = Combination.new(5, 3)
    assert_equal c1.candidates.size, 10

    c2 = Combination.new(5, 2)
    assert_equal c2.candidates.size, 10
  end

  def test_read
    file = File.new("zones.icpc.3278.txt")
    while true do
      n, r = file.readline.split(" ")
      data = file.readline.split.to_a.map {|i| i.to_i }
      sums = []
      c = Combination.new(n.to_i, r.to_i)
      c.candidates.each do |candidate|
        partial = candidate.inject(0) { |sum, n| sum + data[n-1] }
        sums << [candidate, partial]
      end
      loop_count = file.readline.to_i
      for x in 1..loop_count do
        line = file.readline.split.to_a.map { |i| i.to_i }
        sums.each do |sum|
          union_count = line[0].to_i
          union_value = line.last.to_i
          s1 = sum[0].to_set
          s2 = line[1..union_count].to_set
          sum[1] -= union_value if s1.superset?(s2)
        end
      end
      max_index = 0
      max_sum   = 0
      sums.each_index { |i| 
        max_sum, max_index = sums[i][1], i if sums[i][1] > max_sum 
      }
      pp max_index, max_sum
      break if loop_count == 0
    end
  end
end
