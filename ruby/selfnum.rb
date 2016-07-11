#!/usr/bin/env ruby
# encoding: utf-8

class Fixnum
  def digits
    self.to_s.scan(/./).map { |x| x.to_i }
  end

  def generate
    self + self.digits.sum
  end
end

class Array
  def sum
    self.reduce(:+) 
  end

  def selfnum
    self - map { |num| num.generate } 
  end
end

p [*1...5000].selfnum.sum
