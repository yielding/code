#!/usr/bin/env ruby

def process arr; p arr end

def candidates depth
  c = [[*0..1], [*0..2], [*0..3], [*0..4], [*0..5]]
  c[depth]
end

def backtracking(arr, depth, n)
  arr.length == n \
    ? process(arr) 
    : candidates(depth).each { |c| backtracking(arr.clone.push(c), depth+1, n) }
end

backtracking [], 0, 5
