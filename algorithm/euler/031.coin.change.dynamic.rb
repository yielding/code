#!/usr/bin/env ruby19
require "pp"

#target    = 200
#coin_deno = [1, 2, 5, 10, 20, 50, 100, 200] # denomination 

target    = 5
coin_deno = [1, 3] # denomination 
ways      = [1] + [0] * target

coin_deno.each { |coin|  
  (coin..target).each { |i| 
    ways[i] += ways[i - coin] 
    pp ways
  }
  puts "--------"
}

p ways[target]
