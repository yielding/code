#!/usr/bin/env ruby 

require "pp"

p = proc { |x| x * x }
pp (0..9).map(&p)
