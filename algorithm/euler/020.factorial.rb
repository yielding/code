#!/usr/bin/env ruby

require_relative "numeric_ext"

p 100.factorial
p 100.factorial.to_s
               .scan(/\d/).map {|e| e.to_i }
               .reduce(:+)
