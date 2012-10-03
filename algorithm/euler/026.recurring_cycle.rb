#!/usr/bin/env ruby19

def cyclic_no no, deno
  value, remainders = [], []

  deno.times {
    q, r = no.divmod deno

    nil if r == 0

    value << q
    break if remainders.detect { |i| i == r }
    remainders << r

    no = r * 10
  }
  
  value.shift  # throw away "0."
  value
end

max_length = 0
max_val  = 0

1000.downto(1) { |n|
  next if max_length >= n
  arr = cyclic_no(1, n)
  next if arr.nil?
  if arr.size > max_length
    max_length = arr.size
    max_val  = n
  end
}

p max_val

=begin
require "test/unit"

class TestCyclicNumber < Test::Unit::TestCase
  def test_1_by7
    assert_equal(cyclic_no(1,  3), [3])
    assert_equal(cyclic_no(1,  7), [1, 4, 2, 8, 5, 7])
    assert_equal(cyclic_no(1, 11), [0, 9])
    assert_equal(cyclic_no(1, 13), [0, 7, 6, 9, 2, 3])
    assert_equal(cyclic_no(1, 23), [0, 4, 3, 4, 7, 8, 2, 6, 0, 8, 6, 9, 5, 6, 5, 2, 1, 7, 3, 9, 1, 3 ])
    assert_equal(cyclic_no(1, 29), [0, 3, 4, 4, 8, 2, 7, 5, 8, 6, 2, 0, 6, 8, 9, 6, 5, 5, 1, 7, 2, 4, 1, 3, 7, 9, 3, 1])
    assert_equal(cyclic_no(1, 97), [0, 1, 0, 3, 0, 9, 2, 7,  8, 3, 5, 0, 5, 1, 5, 4, 6, 3, 9, 1, 7, 5, 2, 5, 7, 7, 3, 1, 9, 5, 8, 7, 6, 2, 8, 8, 6, 5, 9, 7, 9, 3, 8, 1, 4, 4, 3, 2,  9, 8, 9, 6, 9, 0, 7, 2, 1, 6, 4, 9, 4, 8, 4, 5,  3, 6, 0, 8, 2, 4, 7, 4, 2, 2, 6, 8, 0, 4, 1, 2,  3, 7, 1, 1, 3, 4, 0, 2,  0, 6, 1, 8, 5, 5, 6, 7])
  end
end

=end
