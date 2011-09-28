#!/usr/bin/env ruby -KU

%w{facets/memoize}.each { |lib| require lib }

class Fixnum
  def next_number
    even? ? self / 2 : self * 3 + 1
  end
end

class Solver
  def collatz_from(num)
    num == 1 ? [1] : [num] + collatz_from(num.next_number)
  end

  memoize :collatz_from

  def find_length(num)
    collatz_from(num).length
  end

  def mx_length(from, to)
    (from..to).to_a.map { |n| find_length(n) }.max
  end
end

require "benchmark"

describe "3N + 1" do
  describe "Fixnum" do
    it "should calculate next number" do
      4.next_number.should == 4/2
    end
  end
end
