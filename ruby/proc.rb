#!/usr/bin/env ruby2.0 

require "pp"

p = proc { |x| x * x }
pp (0..9).map(&p)
