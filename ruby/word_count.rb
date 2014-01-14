#!/usr/bin/env ruby
# encoding: utf-8

def count_frequency word_list
  counts = Hash.new(0)
  for word in word_list
    counts[word] += 1
  end

  counts
end

p count_frequency([:a, :a, :lee, :chang])
