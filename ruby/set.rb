#!/usr/bin/env ruby

require 'set'

set1 = Set.new([:bear, :cat, :dear])

puts set1.include?(:bat)

set1.add(:fox)
partition = set1.classify { |element| element.to_s.length }
p partition
