#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"

Chunk = Struct.new(:offset, :count)

def find_start_offset(arr, start)
  sum = 0
  arr.each.with_index do |chk, index|
    sum += chk.count
    return index, chk.count - (sum - start) if sum > start
  end
end

def find_end_offset(arr, start, count)
  index, offset = find_start_offset(arr, start)

  return index, count if offset + count <= arr[index].count

  count -= arr[index].count - offset
  for i in index+1...arr.length
    if count - arr[i].count < 0
      return i, count
    end

    count -= arr[i].count
  end
end

def find_range(arr, start, count)
  index, offset = find_start_offset(arr, start)
  return index, offset, index, count if offset + count <= arr[index].count

  count -= arr[index].count - offset
  for i in index+1...arr.length
    return index, offset, i, count if count - arr[i].count < 0
    count -= arr[i].count
  end
end

def find_range2(arr, start, count)
  c0, o0 = find_start_offset(arr, start)
  c1, o1 = find_start_offset(arr, start + count)
  
  return c0, o0, c1, o1
end


class Testlibrary_file_name < Test::Unit::TestCase
  def setup
    @arr = []
    @arr << Chunk.new(0, 5)
    @arr << Chunk.new(5, 5)
    @arr << Chunk.new(10, 10)
    @arr << Chunk.new(5, 5)
  end

  def test_final
    c0, o0, c1, o1 = find_range2(@arr, 6, 2)
    assert_equal(1, c0); assert_equal(1, o0)
    assert_equal(1, c1); assert_equal(3, o1)

    c0, o0, c1, o1 = find_range2(@arr, 6, 5)
    assert_equal(1, c0); assert_equal(1, o0)
    assert_equal(2, c1); assert_equal(1, o1)
    
    c0, o0, c1, o1 = find_range2(@arr, 6, 16)
    assert_equal(1, c0); assert_equal(1, o0)
    assert_equal(3, c1); assert_equal(2, o1)
  end
  
  def test_find_range
    c0, o0, c1, o1 = find_range(@arr, 6, 17)

    assert_equal(1, c0); assert_equal(1, o0)
    assert_equal(3, c1); assert_equal(3, o1)
  end

  def test_find_start_offset
    c, o = find_start_offset(@arr, 6)
    assert_equal(1, c); assert_equal(1, o)

    c, o = find_start_offset(@arr, 0)
    assert_equal(0, c); assert_equal(0, o)

    c, o = find_start_offset(@arr, 1)
    assert_equal(0, c); assert_equal(1, o)

    c, o = find_start_offset(@arr, 10)
    assert_equal(2, c); assert_equal(0, o)
  end

  def test_find_end_offset
    c, o = find_end_offset(@arr, 0, 3)
    assert_equal(0, c); assert_equal(3, o)

    c, o = find_end_offset(@arr, 1, 5)
    assert_equal(1, c); assert_equal(1, o)

    c, o = find_end_offset(@arr, 5, 5)
    assert_equal(1, c); assert_equal(5, o)

    c, o = find_end_offset(@arr, 6, 17)
    assert_equal(3, c); assert_equal(3, o)

    c, o = find_end_offset(@arr, 5, 0)
    assert_equal(1, c); assert_equal(0, o)
  end
  
end
