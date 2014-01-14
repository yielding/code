#!/usr/bin/env ruby 
# encoding: utf-8

require "pp"

p = proc { |x| x * x }
pp (0..9).map(&p)
