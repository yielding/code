#!/usr/bin/env ruby -wKU

# $:.unshift File.expand_path(File.dirname(__FILE__))

$: << "."
   
require "tt"

t = Test.new
#t.foo = 10

puts t.hello
