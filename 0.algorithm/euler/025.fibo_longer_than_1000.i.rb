#!/usr/bin/env ruby

# This module encapsulates functionality related to the
# generation of Fibonacci sequences.
#--
# Copyright (c) 2004 Dave Thomas, The Pragmatic Programmers, LLC. 
# Licensed under the same terms as Ruby. No warranty is provided.
module Fibonacci

  def Fibonacci.sequence(count, &block)
    result, block = setup_optional_block(block)
    generate do |val|
      break if count <= 0
      count -= 1
      block[val]
    end
    result
  end

  def Fibonacci.upto(max, &block)
    result, block = setup_optional_block(block)
    generate do |val|
      break if val > max
      block[val]
    end
    result
  end

  def Fibonacci.generate
    f1, f2 = 1, 1
    loop do
      yield f1
      f1, f2 = f2, f1 + f2
    end
  end

  def Fibonacci.setup_optional_block(block)
    if block.nil?
      [result = [], lambda { |v| result << v }]
    else
      [nil, block]
    end
  end
end

Fibonacci.upto(10) { |b| puts "intermediate value #{b} " }
