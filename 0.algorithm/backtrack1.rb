#!/usr/bin/env ruby

class Object
  def deep_copy
    Marshal.load(Marshal.dump(self))
  end
end

def process arr
  p arr; true
end

def candidates
  [true, false]
end

def backtracking(arr, depth)
  process(arr) && return if arr.length == depth
  candidates.each { |c| backtracking(arr.clone.push(c), depth) }
end

backtracking [], 4
