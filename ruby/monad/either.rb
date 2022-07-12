#!/usr/bin/env ruby

require_relative './monad'

class Either < Monad

  attr_accessor :is_left, :value

  # pure :: a -> Either a
  def self.pure(value)
    Right.new(value)
  end

  def pure(value)
    self.class.pure(value)
  end

  def flat_map(f)
    if is_left
      self
    else
      f.call(value)
    end
  end
end

class Left < Either
  def initialize(value)
    @value = value
    @is_left = true
  end
end

class Right < Either
  def initialize(value)
    @value = value
    @is_left = false
  end
end
