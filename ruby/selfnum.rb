#!/usr/bin/env ruby2.0

class Fixnum
  def digits
    to_s.scan(/./).map { |x| x.to_i }
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
    self - generate
  end

  private
  def generate
    map { |num| num.generate }
  end

end

p (1...5000).to_a.selfnum.sum
