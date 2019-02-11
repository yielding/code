#!/usr/bin/env ruby

require "pp"

#target    = 200
#coin_deno = [1, 2, 5, 10, 20, 50, 100, 200] # denomination 

target    = 5
coin_deno = [1, 2, 3, 4, 5] # denomination 
ways      = [1] + [0] * target

coin_deno.each { |coin|  
  (coin..target).each { |i| ways[i] += ways[i - coin] }
}

p ways[target]
