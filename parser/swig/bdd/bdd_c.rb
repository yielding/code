#!/usr/bin/env ruby

require "mkmf"

c_program = ARGV.first
`swig -ruby #{c_program}.i`
create_makefile(c_program)
`make`
puts `spec -f s -c #{c_program}_spec.rb`
