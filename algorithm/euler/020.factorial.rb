#!/usr/bin/env ruby

require "numeric_ext"

p 100.factorial
p 100.factorial.to_s
               .scan(/\d/).map {|e| e.to_i }
               .reduce(:+)
