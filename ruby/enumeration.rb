#!/usr/bin/env ruby
# encoding: utf-8

def Integer.all
  Enumerator.new do |yielder, n: 0|
    loop { yielder.yield(n += 1) }
  end.lazy
end

# p Integer.all.first(10)

triangular_numbers = Enumerator.new do |yielder|
  number = 0 
  count  = 1 
  loop do
    number += count
    count  += 1
    yielder.yield number
  end 
end

def infinite_select(enum, &block)
  Enumerator.new do |yielder|
    enum.each do |value|
      yielder.yield(value) if block.call(value)
    end 
  end 
end

p infinite_select(triangular_numbers) { |v| v % 10 == 0}.first(5)
