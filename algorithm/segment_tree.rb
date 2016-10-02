#!/usr/bin/env ruby

require 'pp'

# ti: tree index
# ai: array index

class SegmentTree
  def initialize(arr)
    @arr  = arr
    @size = arr.length
    @tree = [0] * @size * 4

    self.build(0, 0, @size - 1)
  end

  def build(ti, lo, hi)
    if lo == hi
      @tree[ti] = @arr[lo]
      return
    end

    mid = lo + (hi - lo) / 2
    build(2*ti + 1, lo, mid)
    build(2*ti + 2, mid + 1, hi)

    # merge == +
    @tree[ti] = @tree[2*ti + 1] + @tree[2*ti + 2]
  end

  def update(arr_index, val)
    _update(0, 0, @size-1, arr_index, val)
  end

  def query(i, j)
    _query(0, 0, @size-1, i, j)
  end

  def _update(ti, lo, hi, aidx, val)
    if lo == hi
      @tree[ti] = val
      return
    end

    mid = lo + (hi - lo) / 2
    if aidx > mid
      _update(2*ti + 2, mid + 1, hi, aidx, val)
    else 
      _update(2*ti + 1, lo, mid, aidx, val)
    end

    @tree[ti] = @tree[2*ti + 1] + @tree[2 * ti + 2]
  end

  def _query(ti, lo, hi, i, j)
    return 0 if lo > j or hi < i
    return @tree[ti] if i <= lo and j >= hi

    mid = lo + (hi - lo) / 2

    if i > mid
      return _query(2*ti + 2, mid + 1, hi, i, j)
    elsif j <= mid
      return _query(2*ti + 1, lo, mid , i, j)
    end

    lq = query(2*ti + 1, lo, mid, i, mid)
    rq = query(2*ti + 2, mid + 1, hi, mid + 1, j)

    return lq + rq
  end

end

tree = SegmentTree.new [1, 10, 3, 6, 5, 6, 4]
p tree.query(0, 1)
p tree
p tree.update(0, 5)
p tree
