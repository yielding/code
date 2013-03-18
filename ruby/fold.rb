#!/usr/bin/env ruby2.0

def fold (seq, prev, &action)
  if seq.empty?
    prev
  else
    fold(seq, action.call(prev, seq.shift), &action)
  end
end

arr = (1..10).to_a

puts fold(arr, 1) { |a, b| a * b }
