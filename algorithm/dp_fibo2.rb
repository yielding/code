#!/usr/bin/env ruby2.0

class Fibo
  def initialize(n=100)
    @data = [0, 1]
    2.upto(n) { |i| @data[i] = @data[i-1] + @data[i-2] }
  end

  def fibo(n)
    return @data[n] unless @data[n].nil?
    @data[n] = fibo(n-1) + fibo(n-2)
  end
end

f = Fibo.new
p (1..10000).reduce { |s, n| s + f.fibo(n) }
            .to_s.scan(/./)
            .reduce(0) { |sum, n| sum + n.to_i }
