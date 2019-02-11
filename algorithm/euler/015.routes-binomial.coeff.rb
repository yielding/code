#!/usr/bin/env ruby

grid_size = 20

paths = 1
for i in 0...grid_size
  paths *= 2 * grid_size - i
  paths /= i + 1
end

p paths
