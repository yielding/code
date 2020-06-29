#!/usr/bin/env ruby

require 'pp'

# ti: tree index
# ai: array index

class SegmentTree
  def initialize(arr)
    @size = arr.length
    @tree = [0] * (@size * 4)
    @lazy = [0] * (@size * 4)

    build_tree(arr, 0, 0, @size - 1)
  end

  def build_tree(arr, ti, lo, hi)
    if lo == hi
      @tree[ti] = arr[lo]
      return
    end

    mid = lo + (hi - lo) / 2
    build_tree(arr, 2*ti + 1, lo, mid)
    build_tree(arr, 2*ti + 2, mid + 1, hi)

    # merge == +
    @tree[ti] = @tree[2*ti + 1] + @tree[2*ti + 2]
  end

  def update(arr_index, val)
    update_tree(0, 0, @size - 1, arr_index, val)
  end

  def update_range(i, j, val)
    update_lazy_tree(0, 0, @size - 1, i, j, val)
  end

  def query(i, j)
    query_tree(0, 0, @size - 1, i, j)
  end

  def query_lazy(i, j)
    query_lazy_tree(0, 0, @size - 1, i, j) end 
  def traverse
    arr = []
    traverse_tree(arr, 0, 0, @size - 1)
    arr end
private
  def traverse_tree(res, ti, lo, hi, &block)
    if lo == hi
      res << @tree[ti] 
      return
    end

    mid = lo + (hi - lo) / 2
    traverse_tree(res, 2*ti + 1, lo, mid)
    traverse_tree(res, 2*ti + 2, mid + 1, hi)
  end

  def update_tree(ti, lo, hi, ai, val)
    if lo == hi                             # leaf node
      @tree[ti] = val                       # update value.
      return
    end

    mid = lo + (hi - lo) / 2
    if ai <= mid
      update_tree(2*ti + 1, lo, mid, ai, val)
    else 
      update_tree(2*ti + 2, mid + 1, hi, ai, val)
    end

    @tree[ti] = @tree[2*ti + 1] + @tree[2*ti + 2] # merge update
  end

  def query_tree(ti, lo, hi, i, j)
    return 0 if lo > j or hi < i            # segment outside range
    return @tree[ti] if i <= lo and j >= hi # segment inside range

    mid = lo + (hi - lo) / 2                # partial overlap of current segment
                                            # and queried range.
    if i > mid
      return query_tree(2*ti + 2, mid + 1, hi, i, j)
    elsif j <= mid
      return query_tree(2*ti + 1, lo, mid , i, j)
    end

    # 걸쳐저있는 경우
    return query_tree(2*ti + 1, lo, mid, i, mid) +
           query_tree(2*ti + 2, mid + 1, hi, mid + 1, j)
  end

  def query_lazy_tree(ti, lo, hi, i, j)
    return 0 if i > hi or j < lo

    if @lazy[ti] != 0
      @tree[ti] += (hi - lo + 1) * @lazy[ti]
      if lo != hi
        @lazy[2*ti + 1] += @lazy[ti]
        @lazy[2*ti + 2] += @lazy[ti]
      end

      @lazy[ti] = 0
    end

    return @tree[ti] if i <= lo and j >= hi

    mid = lo + (hi - lo) / 2
    return query_lazy_tree(2*ti + 1, lo, mid, i, j)     if j <= mid
    return query_lazy_tree(2*ti + 2, mid + 1, hi, i, j) if i > mid

    lq = query_lazy_tree(2*ti + 1, lo, mid + i, i, mid)
    rq = query_lazy_tree(2*ti + 2, mid + 1, hi, mid + 1, j)

    return lq + rq
  end

  def update_lazy_tree(ti, lo, hi, i, j, val)
    if @lazy[ti] != 0
      @tree[ti] += (hi - lo + 1) * @lazy[ti]
      if lo != hi
        @lazy[2*ti + 1] += @lazy[ti]
        @lazy[2*ti + 2] += @lazy[ti]
      end

      @lazy[ti] =0
    end

    return if lo > hi or lo > j or hi < i

    if i<= lo and hi <= j
      @tree[ti] += (hi - lo + 1) * val

      if lo != hi
        @lazy[2*ti + 1] += val
        @lazy[2*ti + 2] += val
      end

      return
    end

    mid = lo + (hi - lo) / 2

    update_lazy_tree(2*ti + 1, lo, mid, i, j, val)
    update_lazy_tree(2*ti + 2, mid + 1, hi, i, j, val)

    @tree[ti] = @tree[2*ti + 1] + @tree[2*ti + 2]
  end

end

tree = SegmentTree.new [1, 10, 3, 6, 5, 6, 4]
p "#{tree.query(0, 6)} : #{tree.traverse}"

tree.update(6, 2)
p "#{tree.query(0, 6)} : #{tree.traverse}"

tree.update_range(1, 3, 2)
p "#{tree.query(0, 6)} : #{tree.traverse}"

tree.query_lazy(2, 2)
tree.query_lazy(3, 3)
p "#{tree.query(0, 6)} : #{tree.traverse}"
