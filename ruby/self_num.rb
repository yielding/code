#!/usr/bin/env ruby

class Fixnum
  def digits
    to_s.scan(/./).map{ |x| x.to_i }
  end

  def generate
    self + self.digits.sum
  end
end

class Array
  def sum
    inject{|s,v| s+=v}
  end

  def geneach
    map { |num| num.generate }
  end

  def selfnum
    self - geneach
  end
end

p 1227365==(1..5000).to_a.selfnum.sum 
