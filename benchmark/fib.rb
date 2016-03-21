#!/usr/bin/env ruby
#
def fib(n)
  if n == 0 || n == 1
    n
  else
    fib(n-1) + fib(n-2)
  end
end

36.times { |i| puts "n=#{i} => #{fib(i)}" }
