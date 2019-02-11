#!/usr/bin/env ruby

class Numeric
  def sum_of_fifth_power
    self.to_s.scan(/\d/).map { |e| e.to_i ** 5 }.reduce(&:+)
  end
end

p (2..355000).select { |n| n.sum_of_fifth_power == n }.reduce(:+)

=begin rdoc
t = Time.now
arr = []
for i in 2..355000
  sum = 0
  val = i
  while val > 0
    r = val % 10
    sum += r ** 5
    val /= 10
  end

  arr << sum if sum == i
end

p arr.reduce(:+)
p Time.now - t
=end
