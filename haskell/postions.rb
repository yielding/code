#!/usr/bin/env ruby

def positions_of(x, in_:)
    in_.zip([*0..in_.length-1])
      .select { |p| p[0] == x }
      .map    { |p| p[1] }
end

p positions_of(2, in_: [1, 2, 3, 4, 5, 2])
