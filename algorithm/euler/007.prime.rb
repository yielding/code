#!/usr/bin/env ruby -wKU

require "numeric_ext"

def nth_prime? n
  i = 0
  current = 1
  while i<n
    if current.is_prime?
      i += 1
      puts "#{i} th => #{current}" if i % 100 == 0
    end
    current += 1
  end
  current-1
end

puts  nth_prime? 100001
