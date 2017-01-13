#!/usr/bin/env ruby

def positions_of(x, inside:)
  xs = inside

  xs.zip([*0..xs.length-1])
    .select { |p| p[0] == x }
    .map    { |p| p[1] }
end

p positions_of(2, inside: [1, 2, 3, 4, 5, 2])
