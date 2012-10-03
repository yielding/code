#!/usr/bin/env ruby19

(0..9).to_a.permutation(10).each_with_index { |p, index|
  if index.eql?(1000000-1)
    p p.join
    break
  end
}
