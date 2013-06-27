#!/usr/bin/env ruby

class PipelineElement
  attr_accessor :source
  
  def initialize(&block)
    @transformer ||= method(:transform)
    @filter ||= method(:filter)
    @fiber_delegate = Fiber.new do
      process
    end
  end
  
  def | (other=nil, &block)
    other = Transformer.new(&block) if block
    other.source = self
    other
  end
  
  def resume
    @fiber_delegate.resume
  end
  
  def process
    while value = input
      handle_value(@transformer.call(value))
    end
  end
  
  def handle_value(value)
    output(@transformer.call(value)) if @filter.call(value)
  end
  
  def transform(value)
    value
  end
  
  def filter(value)
    true
  end
  
  def input
    source.resume
  end
  
  def output(value)
    Fiber.yield(value)
  end
end

class Transformer < PipelineElement
  def initialize(&block)
    @filter = block
    super
  end
end

class Filter < PipelineElement
  def initialize(&block)
    @filter = block
    super
  end
end

if __FILE__ == $PROGRAM_NAME
  class Evens < PipelineElement
    def process
      value = 0
      loop do
        output(value)
        value += 2
      end
    end
  end

  evens       = Evens.new
  tripler     = Transformer.new { |val| val * 3 }
  incrementer = Transformer.new { |val| val + 1 }
  multiple_of_five = Filter.new { |val| val % 5 == 0 }

  5.times do 
    puts (evens | tripler | incrementer | multiple_of_five).resume
  end

  5.times do
    puts (evens 
      .| { |v| v * 3 } 
      .| { |v| v + 1 } 
      .| multiple_of_five).resume 
  end
end
