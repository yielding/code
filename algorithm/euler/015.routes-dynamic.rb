#!/usr/bin/env ruby19

size = 21
grid = [[0] * size] * size

0.upto(20) { |i| grid[size-1][i] = grid[i][size-1] = 1 }

(size-2).downto(0) { |i|
  (size-2).downto(0) { |j| 
    grid[i][j] = grid[i+1][j] + grid[i][j+1] }
}

p grid[0][0]
