#!/usr/bin/env ruby

class Fibo
  @@cache = {1=>1, 2=>1}

  def fibo(n)
    return 1 if n == 1 or n == 2
    n1 = @@cache[n-1].nil? ? fibo(n-1) : @@cache[n-1]
    n2 = @@cache[n-2].nil? ? fibo(n-2) : @@cache[n-2]
    @@cache[n] = n1 + n2
    @@cache[n]
  end
end

f = Fibo.new
2.upto(6000) { |p|
  s = f.fibo(p).to_s
  if s.size >= 1000
    puts "answer: #{p}"
    exit
  end
}

