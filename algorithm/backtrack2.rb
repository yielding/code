#!/usr/bin/env ruby -wKU

class Object
  def deep_copy;
    Marshal.load(Marshal.dump(self))
  end
end

def process arr
  p arr
end

def candidates depth
  c = [[*0..1], [*0..2], [*0..3], [*0..4], [*0..5]]
  return c[depth]
end

def backtracking(arr, depth, n)
  arr.length == n ? process(arr) 
                  : candidates(depth).each { |c| backtracking(arr.deep_copy.push(c), depth+1, n) }
end

backtracking [], 0, 5
